package com.veadan.folib.ws.common;

import com.alibaba.fastjson.JSONObject;
import com.google.common.util.concurrent.RateLimiter;
import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.components.ws.WSForwardComponent;
import com.veadan.folib.config.PromotionConfig;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.domain.ArtifactDispatch;
import com.veadan.folib.domain.PrivilegeDispatch;
import com.veadan.folib.event.privilege.PrivilegeEventTypeEnum;
import com.veadan.folib.promotion.KryoSerializationUtil;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.util.FileSizeConvertUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.server.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.access.intercept.RunAsManager;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.websocket.*;
import java.io.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URI;
import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

/**
 * @author pengYongQiang
 * @date 2024/2/12 23:09
 */
@Slf4j
@Component
public class FolibWsRunManageV2 {

    private Map<String, Session> FOLIB_WS_RUN_MAP = new ConcurrentHashMap<>();
    public static final String FOLIB_WS_PROTOCOL = "folib_WS_protocol";
    private Map<Session, Long> sessionIdleMap = new ConcurrentHashMap<>();//
    private ConcurrentHashMap<Session, Map<String, CompletableFuture<WSMessageResponse>>> REQUEST_FUTURES = new ConcurrentHashMap<>();
    private ConcurrentHashMap<String, RateLimiter> RATE_LIMITER_MAP = new ConcurrentHashMap<>();
    private ConcurrentHashMap<String, ReentrantLock> REENTRANT_LOCK__MAP = new ConcurrentHashMap<>();

    @Inject
    protected ConfigurationManager configurationManager;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    private ThreadPoolTaskExecutor asyncWsHeartbeatThreadPoolTaskExecutor;

    @Autowired
    private PromotionConfig promotionConfig;

    @Autowired
    private CommonComponent commonComponent;

    @Autowired
    private WSForwardComponent wsForwardComponent;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    private WebSocketContainer webSocketContainer;

    @PostConstruct
    public void init() {
        webSocketContainer = ContainerProvider.getWebSocketContainer();
        webSocketContainer.setDefaultMaxSessionIdleTimeout(promotionConfig.getWsMaxSessionIdleTimeout());
    }

    @Scheduled(cron = "0/5 * * * * ?")
    public void wsContainerTask() {
        // 初始化连接到集群服务端
        final Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
        clusterDispatchNode.values()
                .forEach(clusterDispatchNodeDto -> {
                    asyncWsHeartbeatThreadPoolTaskExecutor.execute(() -> reconnectAndHeartbeat(clusterDispatchNodeDto));
                });
    }

    public void reconnectAndHeartbeat(ClusterDispatchNodeDto nodeInfo) {
        if (null != nodeInfo.getAutoRegister() && nodeInfo.getAutoRegister()) {
            //自动注册的节点不处理，直接返回
            return;
        }
        String targetHostName = FolibWsRunManageUtil.getTargetHostName(nodeInfo);
        ReentrantLock reentrantLock = REENTRANT_LOCK__MAP.computeIfAbsent(targetHostName, s -> new ReentrantLock(true));

        boolean tryLock = reentrantLock.tryLock();
        try {
            if (tryLock) {
                Session session = FOLIB_WS_RUN_MAP.get(targetHostName);
                if (session != null && session.isOpen()) {
                    log.info("TargetNode [{}} sessionId [{}]", targetHostName, session.getId());
                }
                synchronized (targetHostName) {
                    if (Objects.isNull(session) || !session.isOpen()) {
                        try {
                            connectToServerV2(nodeInfo);
                        } catch (Exception e) {
                            log.error("ConnectToServer fail retry...", e);
                        }
                        return;
                    }
                }
                Long latestSendTime = sessionIdleMap.get(session);
                if (latestSendTime != null) {
                    long idleTime = System.currentTimeMillis() - latestSendTime;
                    //检查上次发送数据时间到现在是否已超过了wsHeardBeatIdleTime时间，已超过时间则发送心跳，未超过则不处理
                    if (idleTime < promotionConfig.getWsHeardBeatIdleTime()) {
                        return;
                    }
                    log.info("Send WS HEARD_BEAT [{}]", targetHostName);
                    try {
                        WSMessageResponse wsMessageResponse = sendRequest(targetHostName, new WSMessageRequest(Command.HEARD_BEAT));
                    } catch (Exception e) {
                        log.error(String.format("Ping exception close session [%s]", session), e);
                        try {
                            session.close(new CloseReason(CloseReason.CloseCodes.NORMAL_CLOSURE, "HEARD_BEAT timeout"));
                        } catch (Exception ex) {
                            log.error("Close exception", e);
                        }
                    }
                }
            }
        } finally {
            if (tryLock) {
                reentrantLock.unlock();
            }
        }
    }

    public Session connectToServerV2(ClusterDispatchNodeDto nodeInfo) throws DeploymentException, IOException {
        log.info("Connect WS node info [{}]", JSONObject.toJSONString(nodeInfo));
        FolibWsClient folibWsClient = new FolibWsClient(nodeInfo, configurationManager);
        return webSocketContainer.connectToServer(folibWsClient, URI.create(folibWsClient.getUri()));
    }

    public void registerSession(String targetHostName, Session session) {
        synchronized (targetHostName.intern()) {
            if (!session.isOpen()) {
                throw new IllegalStateException("Registration of unopened sessions is not allowed");
            }
            FOLIB_WS_RUN_MAP.put(targetHostName, session);
            sessionIdleMap.put(session, System.currentTimeMillis());
            log.info("RegisterSession targetHostName [{}] session [{}] []", targetHostName, session);
            commonComponent.putWsNode(getSimpleTargetHostName(targetHostName));
            printWs();
        }
    }

    public void unRegisterSession(String targetHostName, String reason) {
        synchronized (targetHostName.intern()) {
            Session session = FOLIB_WS_RUN_MAP.remove(targetHostName);
            RATE_LIMITER_MAP.remove(getSimpleTargetHostName(targetHostName));
            commonComponent.removeWsNode(getSimpleTargetHostName(targetHostName));
            if (session == null) {
                log.warn("Session is null targetHostName [{}]", targetHostName);
                return;
            }
            log.info("unRegisterSession targetHostName [{}] session [{}] reason [{}]", targetHostName, session, reason);
            sessionIdleMap.remove(session);
            if (session.isOpen()) {
                try {
                    session.close(new CloseReason(CloseReason.CloseCodes.NORMAL_CLOSURE, reason));
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            sessionLastSendTime.remove(session);
            sessionBytesSent.remove(session);
            sessionLocks.remove(session);
            printWs();
        }
    }

    public void cleanFuture(Session session, Throwable error) {
        Map<String, CompletableFuture<WSMessageResponse>> futureMap = REQUEST_FUTURES.get(session);
        if (futureMap != null) {
            for (CompletableFuture<WSMessageResponse> value : futureMap.values()) {
                value.completeExceptionally(error);
            }
        }
    }

    public Session getSession(String targetHostName) {
        return getSession(targetHostName, false);
    }

    public Session getSession(String targetHostName, boolean isAutoRegister) {
        printWs();
        Session session = FOLIB_WS_RUN_MAP.get(targetHostName);
        if (Objects.isNull(session)) {
            targetHostName = getTargetHostName(targetHostName);
            session = FOLIB_WS_RUN_MAP.get(targetHostName);
        }
        if (isAutoRegister && (Objects.isNull(session) || !session.isOpen())) {
            //自动注册的节点并且没有session或者session已经关闭
            unRegisterSession(targetHostName, "active close");
        }
        return session;
    }

    private String getTargetHostName(String targetHostName) {
        String targetNode = getSimpleTargetHostName(targetHostName);
        if (MapUtils.isNotEmpty(FOLIB_WS_RUN_MAP)) {
            Optional<String> optional = FOLIB_WS_RUN_MAP.keySet().stream().filter(item -> getSimpleTargetHostName(item).equals(targetNode)).findFirst();
            if (optional.isPresent()) {
                return optional.get();
            }
        }
        return targetHostName;
    }

    public String getSimpleTargetHostName(String targetHostName) {
        return targetHostName.split("_")[0];
    }

    public String getTargetNode(String targetHostName) {
        Session session = FOLIB_WS_RUN_MAP.get(targetHostName);
        if (Objects.isNull(session)) {
            targetHostName = getTargetHostName(targetHostName);
        }
        if (!FOLIB_WS_RUN_MAP.containsKey(targetHostName)) {
            return "";
        }
        return targetHostName;
    }

    public String getTargetNode(Session session) {
        if (Objects.nonNull(session)) {
            return session.getRequestURI().getPath().substring("/wsv2/folib/".length());
        }
        return "";
    }

    private void sendBinary(String targetNode, Session session, WSMessage wsMessage, long finalKbps) throws Exception {
        ByteBuffer byteBuffer = ByteBuffer.wrap(KryoSerializationUtil.serialize(wsMessage));

        try {
            sendBinaryV2(targetNode, session, byteBuffer, finalKbps);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        //session.getBasicRemote().sendBinary(byteBuffer);

    }
    public WSMessageResponse sendRequest(String targetHostName, Command command) throws Exception {
        return sendRequest(targetHostName, new WSMessageRequest(command));
    }

    public WSMessageResponse sendRequest(String targetHostName, WSMessageRequest wsMessageRequest) throws FolibWsRequestException {
        return sendRequest(targetHostName, wsMessageRequest, promotionConfig.getWsRequestTimout());
    }

    public WSMessageResponse sendRequest(String targetHostName, WSMessageRequest wsMessageRequest, int timeout) throws FolibWsRequestException {

        CompletableFuture<WSMessageResponse> future = new CompletableFuture<>();
        Session session = getSession(targetHostName);

        if (session == null) {
            throw new RuntimeException("Not found session with targetHostName:" + targetHostName);
        }
        if (!session.isOpen()) {
            Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
            ClusterDispatchNodeDto clusterDispatchNodeDto = clusterDispatchNode.values().stream().filter(dto -> getSimpleTargetHostName(targetHostName).equals(FolibWsRunManageUtil.getSimpleTargetHostName(dto))).findAny().orElse(null);
            if (Objects.isNull(clusterDispatchNodeDto)) {
                throw new RuntimeException("Not found session with targetHostName:" + targetHostName);
            }
            log.warn("Session is closed reconnectAndHeartbeat [{}]", JSONObject.toJSONString(clusterDispatchNodeDto));
            reconnectAndHeartbeat(clusterDispatchNodeDto);
            session = getSession(targetHostName, clusterDispatchNodeDto.getAutoRegister());
        }
        if (!session.isOpen()) {
            throw new RuntimeException("Session is closed with targetHostName:" + targetHostName);
        }

        final long finalKbps = getRateKbps(targetHostName);
        Map<String, CompletableFuture<WSMessageResponse>> futureMap = REQUEST_FUTURES.computeIfAbsent(session, session1 -> new ConcurrentHashMap<>());
        futureMap.put(wsMessageRequest.getId(), future);
        try {

            log.info("WsMessageRequest [{}]", wsMessageRequest);
            sendBinary(targetHostName, session, wsMessageRequest, finalKbps);
        } catch (Exception e) {
            log.error("SendBinary fail", e);
            future.completeExceptionally(e);
        }
        final WSMessageResponse wsMessageResponse;
        try {//todo 出现异常将其关闭
            wsMessageResponse = future.get(timeout, TimeUnit.SECONDS);
        } catch (Exception e) {
            throw new FolibWsRequestException(e);
        } finally {
            futureMap.remove(wsMessageRequest.getId());
        }
        return wsMessageResponse;
    }

    public void sendResponse(String nodeName, Session session, WSMessageResponse wsMessageResponse) throws Exception {
        final long finalKbps = getRateKbps(nodeName);
        sendBinary(nodeName, session, wsMessageResponse, finalKbps);
    }

    public CompletableFuture<WSMessageResponse> getFuture(Session session, String requestId) {
        Map<String, CompletableFuture<WSMessageResponse>> futureMap = REQUEST_FUTURES.get(session);
        if (futureMap != null) {
            return futureMap.get(requestId);
        }
        return null;
    }


    private static final long _1_MB = 1024 * 1024; // 1MB

    /**
     * 限速缺省值50M
     */
    private static final long DEFAULT_BYTES_PER_SECOND = _1_MB * 50;
    private final Map<Session, Long> sessionLastSendTime = new ConcurrentHashMap<>();
    private final Map<Session, Long> sessionBytesSent = new ConcurrentHashMap<>();
    private final Map<Session, ReentrantLock> sessionLocks = new ConcurrentHashMap<>();

    private final String PROMOTION_CHUNK_SIZE_KEY = "PROMOTION_CHUNK_SIZE";


    public void sendBinaryV2(String targetNode, Session session, ByteBuffer data, long finalKbps) throws IOException {
        String messageId = UUID.randomUUID().toString();

        if (finalKbps <= 0) {
            finalKbps = DEFAULT_BYTES_PER_SECOND;
        }

        long finalKbps1 = finalKbps;
        RateLimiter rateLimiter = RATE_LIMITER_MAP.computeIfAbsent(getSimpleTargetHostName(targetNode), s -> RateLimiter.create(finalKbps1));
        rateLimiter.setRate(finalKbps);

        int dataSize = data.remaining();
        long startTime = System.currentTimeMillis();

        log.info("About to sendBinary targetNode [{}] messageId [{}] sessionId [{}] finalKbps [{}] size [{}]", targetNode, messageId, session.getId(), finalKbps, dataSize);

        ReentrantLock reentrantLock = sessionLocks.computeIfAbsent(session, session1 -> new ReentrantLock(true));
        int sendBytesCount = 0;
        String  cacheChunkSize = distributedCacheComponent.get(PROMOTION_CHUNK_SIZE_KEY);
        int minimumPacketSize = cacheChunkSize == null ? 10*1024 * 1024 : Integer.parseInt(cacheChunkSize);
        //TCP通道一次只能处理一个消息，每次发送1M数据，为了解决TCP  队头阻塞，如果实际网络较小，发送过大的数据，会导致时间变长，比如发送20M，实际网络1M，则需要20S，在这个时间内会阻碍其他WS消息处理，心跳超时，会导致主动断开WS通道
        if (minimumPacketSize > finalKbps) {
            minimumPacketSize = (int) finalKbps;
        }

        // 将ByteBuffer转换为InputStream
        byte[] dataArray = new byte[data.remaining()];
        data.get(dataArray);
        ByteArrayInputStream inputStream = new ByteArrayInputStream(dataArray);

        // 预先计算协议字节和消息ID字节
        byte[] protocolBytes = FOLIB_WS_PROTOCOL.getBytes();
        byte[] messageIdBytes = messageId.getBytes();

        while (inputStream.available() > 0) {
            long lockStartTime = System.currentTimeMillis();
            reentrantLock.lock();
            //公平锁保证多线程，同一个session按照顺序发送WS消息，如果不加，并发时可能乱序处理，可能和操作系统有关，导致最开始的消息一直未发送，发送超时后，会导致WS通道断开
            try {
                sessionIdleMap.put(session, System.currentTimeMillis());
                int chunkSize = Math.min(inputStream.available(), minimumPacketSize);

                ByteBuffer chunk = ByteBuffer.allocate(chunkSize + 8 + protocolBytes.length + messageIdBytes.length);
                chunk.put(protocolBytes);
                chunk.putLong(dataSize);
                chunk.put(messageIdBytes);

                byte[] chunkArray = new byte[chunkSize];
                // 流中读取数据
                int bytesRead = inputStream.read(chunkArray);
                if (bytesRead != chunkSize) {
                    throw new IOException("Unexpected end of input stream");
                }
                chunk.put(chunkArray);
                // 准备读取
                chunk.flip();
                //尝试从rateLimiter中获取chunkSize个许可，如果无法获取到，就会阻塞，上传超时时间10分钟，委托上传超时时间60分钟，发送超时后，会导致WS通道断开
                double timeConsuming = rateLimiter.acquire(chunkSize);
                log.info("RateLimiter targetNode [{}] messageId [{}] sessionId [{}] rate [{}] acquire [{}] time consuming [{} s] ...", targetNode, messageId, session.getId(), (long) rateLimiter.getRate(), chunkSize, timeConsuming);

                CompletableFuture<Void> completableFuture = new CompletableFuture<>();
                RemoteEndpoint.Async asyncRemote = session.getAsyncRemote();
                asyncRemote.setSendTimeout(3);
                asyncRemote.sendBinary(chunk, result -> {
                    if (result.isOK()) {
                        // 完成Future
                        completableFuture.complete(null);
                    } else {
                        // 完成Future并传递异常
                        completableFuture.completeExceptionally(result.getException());
                    }
                });

                try {
                    // 阻塞等待直到Future完成
                    completableFuture.get();
                } catch (Exception ex) {
                    throw new RuntimeException(ex);
                }

                sendBytesCount += chunk.capacity();
                long pastTime = System.currentTimeMillis() - startTime;

                BigDecimal rate = BigDecimal.valueOf(0);
                try {
                    BigDecimal second = BigDecimal.valueOf(pastTime).divide(BigDecimal.valueOf(1000), 2, RoundingMode.HALF_UP);
                    rate = BigDecimal.valueOf(sendBytesCount).divide(second, 2, RoundingMode.HALF_UP);
                } catch (Exception ignored) {
                }

                log.info("Send targetNode [{}] messageId [{}] sessionId [{}] already send [{}] remaining [{}] finalKbps [{} ps] ...", targetNode, messageId, session.getId(), dataSize - inputStream.available(), dataSize, FileSizeConvertUtils.convert(rate.longValue()));

                if (inputStream.available() <= 0) {
                    log.info("Send finished targetNode [{}] messageId [{}] sessionId [{}] already send [{}] time consuming [{} ms] lock info {} ]", targetNode, messageId, session.getId(), dataSize, System.currentTimeMillis() - startTime, reentrantLock.toString());
                }
            } finally {
                reentrantLock.unlock();
                log.info("RateLimiter targetNode [{}] messageId [{}] sessionId [{}] lock unlock time consuming [{} ms]]...", targetNode, messageId, session.getId(), System.currentTimeMillis() - lockStartTime);
            }
        }
    }

    private long getRateKbps(String targetHostName) {
        final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getKbps()).orElse(0) * (1024L);
        final Collection<ClusterDispatchNodeDto> clusterDispatchNodeDtos = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().values();
        final Map<String, Long> nodeKbpsMap = clusterDispatchNodeDtos.stream().collect(Collectors.toMap(e -> String.format("%s:%s", UrlUtils.getHost(e.getClusterNodeHost()), UrlUtils.getPort(e.getClusterNodeHost())), e -> null != e.getKbps() ? e.getKbps() * 1024L : 0L));
        return Optional.ofNullable(nodeKbpsMap.get(FolibWsRunManageUtil.resolverTargetHostName(targetHostName))).filter(k -> k > 0).orElse(kbps);
    }

    public String printWs() {
        if (MapUtils.isNotEmpty(FOLIB_WS_RUN_MAP)) {
            String wsMap = String.join(",", FOLIB_WS_RUN_MAP.keySet());
            log.info("Current WS map [{}]", wsMap);
            return wsMap;
        }
        log.warn("Current WS map is null");
        return "";
    }

    public boolean forward(String targetNodeName) {
        try {
            Session session = getSession(targetNodeName);
            if (session == null || !session.isOpen()) {
                return wsForwardComponent.forward(targetNodeName);
            }
            return false;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public boolean dispatch(String targetNodeName, ArtifactDispatch artifactDispatch) {
        try {
            Session session = getSession(targetNodeName);
            if (session == null || !session.isOpen()) {
                return wsForwardComponent.dispatch(targetNodeName, artifactDispatch);
            }
            return false;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    public boolean dispatch(String targetNodeName, PrivilegeDispatch privilegeDispatch) {
        try {
            Session session = getSession(targetNodeName);
            if (session == null || !session.isOpen()) {
                return wsForwardComponent.dispatch(targetNodeName, privilegeDispatch);
            }
            return false;
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }
}

class FolibWsRequestException extends Exception {
    public FolibWsRequestException(String message) {
        super(message);
    }

    public FolibWsRequestException(Exception e) {
        super(e);
    }
}
