package com.veadan.folib.ws.common;

import com.google.common.util.concurrent.RateLimiter;
import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.config.PromotionConfig;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.promotion.KryoSerializationUtil;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.util.FileSizeConvertUtils;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.server.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.websocket.*;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URI;
import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.*;
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
    private ConcurrentHashMap<Session, RateLimiter> RATE_LIMITER_MAP = new ConcurrentHashMap<>();
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
    private DistributedLockComponent distributedLockComponent;
    private WebSocketContainer webSocketContainer;

    @PostConstruct
    public void init() {
        webSocketContainer = ContainerProvider.getWebSocketContainer();
        webSocketContainer.setDefaultMaxSessionIdleTimeout(promotionConfig.getWsMaxSessionIdleTimeout());
    }

    @Scheduled(cron = "0/5 * * * * ?")
    public void wsContainerTask() {
//        String lockKey = "WS_CONTAINER_KEY";
//        if (distributedLockComponent.lock(lockKey, 30, TimeUnit.SECONDS, 9999, TimeUnit.DAYS)) {
//            // 初始化连接到集群服务端
//            final Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
//            clusterDispatchNode.values()
//                    .forEach(clusterDispatchNodeDto -> {
//                        asyncWsHeartbeatThreadPoolTaskExecutor.execute(() -> reconnectAndHeartbeat(clusterDispatchNodeDto));
//                    });
//        } else {
//            log.warn("WsContainerTask [{}] was not get lock", lockKey);
//        }
    }

    public void reconnectAndHeartbeat(ClusterDispatchNodeDto nodeInfo) {
        if (null != nodeInfo.getAutoRegister() && nodeInfo.getAutoRegister()) {
            //自动注册的节点不处理，直接返回
            return;
        }
        String targetHostName = FolibWsRunManageUtil.getTargetHostName(nodeInfo);
        ReentrantLock reentrantLock = REENTRANT_LOCK__MAP.computeIfAbsent(targetHostName, s -> new ReentrantLock());

        boolean tryLock = reentrantLock.tryLock();
        try {
            if (tryLock) {
                Session session1 = FOLIB_WS_RUN_MAP.get(targetHostName);
                synchronized (targetHostName) {
                    if (!(session1 != null && session1.isOpen())) {
                        try {
                            connectToServerV2(nodeInfo);
                        } catch (DeploymentException | IOException e) {
                            log.error("connectToServer fail , retry...", e);
                        }
                        return;
                    }
                }
                Long l = sessionIdleMap.get(session1);
                if (l != null) {
                    long idleTime = System.currentTimeMillis() - l;
                    if (idleTime < promotionConfig.getWsHeardBeatIdleTime()) {
                        return;
                    }
                    log.info("send ws HEARD_BEAT {}", targetHostName);
                    try {
                        WSMessageResponse wsMessageResponse = sendRequest(targetHostName, new WSMessageRequest(Command.HEARD_BEAT));
                    } catch (Exception e) {
                        log.error(String.format("ping Exception,close session:%s", session1), e);
                        try {
                            session1.close(new CloseReason(CloseReason.CloseCodes.NORMAL_CLOSURE, "HEARD_BEAT timeout"));
                        } catch (IOException ex) {
                            log.error("close exception", e);
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

//    public void startHeartbeat(String targetHostName) {
//        log.info("startHeartbeat targetHostName:{}", targetHostName);
//        ScheduledFuture<?> scheduledFuture = scheduledExecutorService.scheduleAtFixedRate(() -> {
//            log.info("send Heartbeat to targetHostName:{}", targetHostName);
//            CompletableFuture<WSMessageResponse> future = sendRequest(targetHostName, new WSMessageRequest(Command.HEARD_BEAT));
//            try {
//                WSMessageResponse wsMessageResponse = future.get(2, TimeUnit.SECONDS);
//            } catch (InterruptedException e) {
//                log.info("Interrupted Heartbeat", e);
//            } catch (ExecutionException | TimeoutException e) {
//                reconnect(targetHostName);
//            }
//        }, 10, 10, TimeUnit.SECONDS);
//        heartbeatFutures.put(targetHostName, scheduledFuture);
//    }
//
//    public void stopHeartbeat(String targetHostName) {
//        log.info("stopHeartbeat targetHostName:{}", targetHostName);
//        ScheduledFuture<?> future = heartbeatFutures.remove(targetHostName);
//        if (future == null) {
//            return;
//        }
//        future.cancel(true);
//    }


    public Session connectToServerV2(ClusterDispatchNodeDto nodeInfo) throws DeploymentException, IOException {
        log.info("connect ws nodeInfo:{}", nodeInfo);
        FolibWsClient folibWsClient = new FolibWsClient(nodeInfo, configurationManager);
        return webSocketContainer.connectToServer(folibWsClient, URI.create(folibWsClient.getUri()));
    }

    public void registerSession(String targetHostName, Session session) {
        synchronized (targetHostName.intern()) {
            if (!session.isOpen()) {
                throw new IllegalStateException("registration of unopened sessions is not allowed");
            }
            log.info("registerSession [targetHostName:{} session:{}]", targetHostName, session);
            FOLIB_WS_RUN_MAP.put(targetHostName, session);
            sessionIdleMap.put(session, System.currentTimeMillis());
        }
    }

    public void unRegisterSession(String targetHostName, String reason) {
        synchronized (targetHostName.intern()) {
            Session session = FOLIB_WS_RUN_MAP.remove(targetHostName);
            if (session == null) {
                log.warn("session is null , targetHostName:{}", targetHostName);
                return;
            }
            log.info("unRegisterSession [targetHostName:{} session:{}]", targetHostName, session);
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
            RATE_LIMITER_MAP.remove(session);
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
        return FOLIB_WS_RUN_MAP.get(targetHostName);
    }

    private void sendBinary(Session session, WSMessage wsMessage, long finalKbps) throws ExecutionException, InterruptedException, TimeoutException {
        ByteBuffer byteBuffer = ByteBuffer.wrap(KryoSerializationUtil.serialize(wsMessage));

        try {
            sendBinaryV2(session, byteBuffer, finalKbps);
        } catch (IOException e) {
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
            throw new RuntimeException("not found session with targetHostName:" + targetHostName);
        }
        if (!session.isOpen()) {
            Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
            ClusterDispatchNodeDto clusterDispatchNodeDto = clusterDispatchNode.values().stream().filter(dto -> {
                return targetHostName.equals(FolibWsRunManageUtil.getTargetHostName(dto));
            }).findAny().orElse(null);
            log.warn("session is closed reconnectAndHeartbeat,{}", clusterDispatchNodeDto);
            reconnectAndHeartbeat(clusterDispatchNodeDto);
            session = getSession(targetHostName);
        }
        if (!session.isOpen()) {
            throw new RuntimeException("session is closed , with targetHostName:" + targetHostName);
        }

        final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getKbps()).orElse(0) * (1024L);

        final Collection<ClusterDispatchNodeDto> clusterDispatchNodeDtos = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().values();
        final Map<String, Long> nodeKbpsMap = clusterDispatchNodeDtos.stream().collect(Collectors.toMap(e -> String.format("%s:%s", UrlUtils.getHost(e.getClusterNodeHost()), UrlUtils.getPort(e.getClusterNodeHost())), e -> null != e.getKbps() ? e.getKbps() * 1024L : 0L));
        final long finalKbps = Optional.ofNullable(nodeKbpsMap.get(targetHostName)).filter(k -> k > 0).orElse(kbps);


        Map<String, CompletableFuture<WSMessageResponse>> futureMap = REQUEST_FUTURES.computeIfAbsent(session, session1 -> new ConcurrentHashMap<>());
        futureMap.put(wsMessageRequest.getId(), future);
        try {
            log.info("wsMessageRequest {}", wsMessageRequest);
            sendBinary(session, wsMessageRequest, finalKbps);
        } catch (Exception e) {
            log.error("sendBinary fail", e);
            future.completeExceptionally(e);
        }
        WSMessageResponse wsMessageResponse;
        try {//todo 出现异常将其关闭
            wsMessageResponse = future.get(timeout, TimeUnit.SECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            throw new FolibWsRequestException(e);
        } finally {
            futureMap.remove(wsMessageRequest.getId());
        }
        return wsMessageResponse;
    }

    public void sendResponse(Session session, WSMessageResponse wsMessageResponse) throws ExecutionException, InterruptedException, TimeoutException {
        sendBinary(session, wsMessageResponse, 0L);
    }

    public CompletableFuture<WSMessageResponse> getFuture(Session session, String requestId) {
        Map<String, CompletableFuture<WSMessageResponse>> futureMap = REQUEST_FUTURES.get(session);
        if (futureMap != null) {
            return futureMap.get(requestId);
        }
        return null;
    }


    private static final long _1_MB = 1024 * 1024; // 1MB


    private static final long DEFAULT_BYTES_PER_SECOND = _1_MB * 50; //缺省值50M
    private final Map<Session, Long> sessionLastSendTime = new ConcurrentHashMap<>();
    private final Map<Session, Long> sessionBytesSent = new ConcurrentHashMap<>();
    private final Map<Session, ReentrantLock> sessionLocks = new ConcurrentHashMap<>();


    private void sendBinaryV2(Session session, ByteBuffer data, long finalKbps) throws IOException {
        String messageId = UUID.randomUUID().toString();
        //缺省填充
        if (finalKbps <= 0) {
            finalKbps = DEFAULT_BYTES_PER_SECOND;
        }
        long finalKbps1 = finalKbps;
        RateLimiter rateLimiter = RATE_LIMITER_MAP.computeIfAbsent(session, s -> RateLimiter.create(finalKbps1));
        rateLimiter.setRate(finalKbps);
        int dataSize = data.remaining();
        int bytesToSend = dataSize;
        long startTime = System.currentTimeMillis();
        log.info("sendBinary [size:{} , finalKbps:{} Kbps, messageId:{}]", bytesToSend, finalKbps, messageId);
        ReentrantLock reentrantLock = sessionLocks.computeIfAbsent(session, session1 -> new ReentrantLock(true));
        int sendBytesCount = 0;
        int minimumPacketsize = 1024 * 1024;
        if (minimumPacketsize > finalKbps) {
            minimumPacketsize = (int) finalKbps;
        }
        while (bytesToSend > 0) {
            reentrantLock.lock();
            try {
                sessionIdleMap.put(session, System.currentTimeMillis());
                int chunkSize = Math.min(bytesToSend, minimumPacketsize);
                // 准备数据包，包括协议头、消息ID和数据
                byte[] bytes = FOLIB_WS_PROTOCOL.getBytes();
                int i1 = chunkSize + 8 + bytes.length + messageId.getBytes().length;

                ByteBuffer chunk = ByteBuffer.allocate(i1);
                chunk.put(bytes);
                chunk.putLong(dataSize);
                chunk.put(messageId.getBytes());
                boolean isLast = bytesToSend == chunkSize; // 检查是否为最后一个片段
                //chunk.put((byte) (isLast ? 1 : 0)); // isLast flag
                rateLimiter.acquire(chunkSize);
                for (int i = 0; i < chunkSize; i++) {
                    chunk.put(data.get());
                }
                // 准备读取
                chunk.flip();
                // session.getBasicRemote().sendBinary(chunk);

                CompletableFuture<Void> completableFuture = new CompletableFuture<>();
                long l = System.currentTimeMillis();
                RemoteEndpoint.Async asyncRemote = session.getAsyncRemote();
                asyncRemote.setSendTimeout(10);
                asyncRemote.sendBinary(chunk, result -> {
                    if (result.isOK()) {
                        completableFuture.complete(null); // 完成Future
                    } else {
                        completableFuture.completeExceptionally(result.getException()); // 完成Future并传递异常
                    }
                });

                try {
                    completableFuture.get(); // 阻塞等待直到Future完成
                } catch (InterruptedException | ExecutionException e) {
                    throw new RuntimeException(e);
                }
                sendBytesCount += chunk.capacity();
                long pastTime = System.currentTimeMillis() - startTime;
                // 减少待发送的数据量
                bytesToSend -= chunkSize;
                //日志输出
                BigDecimal rate = BigDecimal.valueOf(0);
                try {
                    BigDecimal second = BigDecimal.valueOf(pastTime).divide(BigDecimal.valueOf(1000), 2, RoundingMode.HALF_UP);
                    rate = BigDecimal.valueOf(sendBytesCount).divide(second, 2, RoundingMode.HALF_UP);
                } catch (Exception ignored) {
                }
                log.info("messageId:{} dataSize:{}/{}, current finalKbps {}ps", messageId, dataSize - bytesToSend, dataSize, FileSizeConvertUtils.convert(rate.longValue()));
                if (isLast) {
                    log.info("send success , time consuming:{}ms", System.currentTimeMillis() - startTime);
                }
            } finally {
                reentrantLock.unlock();
            }
        }

    }

    private void sendBinary(Session session, ByteBuffer data, long finalKbps) throws IOException {
        String messageId = UUID.randomUUID().toString();
        //缺省填充
        if (finalKbps <= 0) {
            finalKbps = DEFAULT_BYTES_PER_SECOND;
        }

        long dataSize = data.remaining();
        long bytesToSend = dataSize;
        long startTime = System.currentTimeMillis();
        log.info("sendBinary [size:{} , finalKbps:{} Kbps, messageId:{}]", bytesToSend, finalKbps, messageId);
        sessionLocks.putIfAbsent(session, new ReentrantLock(true));
        ReentrantLock lock = sessionLocks.get(session);
        int sendBytesCount = 0;
        while (bytesToSend > 0) {
            lock.lock();
            try {

                long currentTime = System.currentTimeMillis();
                sessionLastSendTime.putIfAbsent(session, currentTime);
                sessionBytesSent.putIfAbsent(session, 0L);
                // 计算自上次发送以来经过的时间（毫秒）
                long elapsedTime = currentTime - sessionLastSendTime.get(session);

                /**
                 * 计算在当前带宽限制下，理论上这段时间内可以发送的字节数
                 * e.g. 1
                 * finalKbps=50Kbps 设定的带宽限制
                 * elapsedTime=2000ms 从上一次发送数据到现在经过了多少时间
                 * sessionBytesSent.get(session)=50k 周期内已经发送的数据
                 *
                 * 50 * 2000 / 1000 - 50 = 50
                 * availableBandwidth=50 可用带宽50
                 *
                 * e.g. 2
                 * 当速率过快情况，即在300ms内消耗完了50Kbps的带宽
                 * finalKbps=50Kbps 设定的带宽限制
                 * elapsedTime=300ms 从上一次发送数据到现在经过了多少时间
                 * sessionBytesSent.get(session)=50k 周期内已经发送的数据
                 *
                 *  50 * 300 / 1000 - 50 = -35
                 *  availableBandwidth=-35 可用带宽-35，带宽不足
                 *  进入休眠，休眠时间为1s周期内剩余时间，即 1000 - elapsedTime(300) = 700 ms
                 */
                long availableBandwidth = finalKbps * elapsedTime / 1000 - sessionBytesSent.get(session);
                // 如果可用带宽不足，等待直到下一个计算周期
                if (availableBandwidth <= 0) {
                    try {
                        TimeUnit.MILLISECONDS.sleep(1000 - elapsedTime);
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                    continue;
                }
                // 计算本次可以发送的数据量，不超过待发送数据量和可用带宽允许的最大值
                int chunkSize1 = (int) Math.min(bytesToSend, availableBandwidth);
                int chunkSize = Math.min(chunkSize1, 1024 * 1024);
                // 准备数据包，包括协议头、消息ID和数据
                byte[] bytes = FOLIB_WS_PROTOCOL.getBytes();
                ByteBuffer chunk = ByteBuffer.allocate(chunkSize + 8 + bytes.length + messageId.getBytes().length);
                chunk.put(bytes);
                chunk.putLong(dataSize);
                chunk.put(messageId.getBytes());
                boolean isLast = bytesToSend == chunkSize; // 检查是否为最后一个片段
                //chunk.put((byte) (isLast ? 1 : 0)); // isLast flag
                for (int i = 0; i < chunkSize; i++) {
                    chunk.put(data.get());
                }
                // 准备读取
                chunk.flip();
                // session.getBasicRemote().sendBinary(chunk);

                CompletableFuture<Void> completableFuture = new CompletableFuture<>();
                long l = System.currentTimeMillis();
                session.getAsyncRemote().sendBinary(chunk, result -> {
                    if (result.isOK()) {
                        completableFuture.complete(null); // 完成Future
                    } else {
                        completableFuture.completeExceptionally(result.getException()); // 完成Future并传递异常
                    }
                });

                try {
                    completableFuture.get(); // 阻塞等待直到Future完成
                } catch (InterruptedException | ExecutionException e) {
                    throw new RuntimeException(e);
                }
                sendBytesCount += chunk.capacity();
                long pastTime = System.currentTimeMillis() - startTime;

                // 更新已发送的数据量
                sessionBytesSent.compute(session, (k, v) -> v + chunkSize);
                // 如果已经到达计算周期（1秒），重置会话状态
                if (elapsedTime >= 1000) {
                    sessionLastSendTime.put(session, currentTime);
                    sessionBytesSent.put(session, 0L);
                }
                // 减少待发送的数据量
                bytesToSend -= chunkSize;

                //日志输出
                BigDecimal rate = BigDecimal.valueOf(0);
                try {
                    BigDecimal second = BigDecimal.valueOf(pastTime).divide(BigDecimal.valueOf(1000), 2, RoundingMode.HALF_UP);
                    rate = BigDecimal.valueOf(sendBytesCount).divide(second, 2, RoundingMode.HALF_UP);
                } catch (Exception ignored) {
                }
                log.info("messageId:{} dataSize:{}/{}, current finalKbps {}ps", messageId, dataSize - bytesToSend, dataSize, FileSizeConvertUtils.convert(rate.longValue()));
                if (isLast) {
                    log.info("send success , time consuming:{}ms", System.currentTimeMillis() - startTime);
                }
            } finally {
                lock.unlock();
            }
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
