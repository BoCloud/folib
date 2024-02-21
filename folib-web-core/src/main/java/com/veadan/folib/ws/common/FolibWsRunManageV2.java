package com.veadan.folib.ws.common;

import cn.hutool.http.HttpUtil;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.promotion.KryoSerializationUtil;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.utils.UrlUtils;
import com.veadan.folib.ws.server.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.websocket.ContainerProvider;
import javax.websocket.DeploymentException;
import javax.websocket.Session;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
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
    private ConcurrentHashMap<String, CompletableFuture<WSMessageResponse>> REQUEST_FUTURES = new ConcurrentHashMap<>();

    @Inject
    protected ConfigurationManager configurationManager;
    @Autowired
    private ConfigurationManagementService configurationManagementService;
    @Autowired
    private Executor asyncThreadPoolTaskExecutor;

    public String getTargetHostName(ClusterDispatchNodeDto nodeInfo) {
        String clusterNodeHost = nodeInfo.getClusterNodeHost();
        return getTargetHostName(clusterNodeHost);
    }

    public String getTargetHostName(String clusterNodeHost) {
        URL destUrl = null;
        try {
            destUrl = new URL(clusterNodeHost);
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
        String destHost = destUrl.getHost();
        Integer destPort = UrlUtils.getPort(clusterNodeHost);
        return String.format("%s:%s", destHost, destPort);
    }


    @Scheduled(cron = "0/5 * * * * ?")
    public void Scheduled() {
        // 初始化连接到集群服务端
        final Map<String, ClusterDispatchNodeDto> clusterDispatchNode = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode();
        clusterDispatchNode.values().stream()
                // 排除自动注册的节点信息
                .filter(e -> null != e.getAutoRegister() && !e.getAutoRegister())
                .forEach(clusterDispatchNodeDto -> {
                    asyncThreadPoolTaskExecutor.execute(() -> reconnectAndHeartbeat(clusterDispatchNodeDto));
                });
    }
    public void reconnectAndHeartbeat(ClusterDispatchNodeDto nodeInfo){
        final String clusterNodeHost = nodeInfo.getClusterNodeHost();
        final URL destUrl;
        final URL originUrl;
        try {
            destUrl = new URL(clusterNodeHost);
            originUrl = new URL(configurationManager.getConfiguration().getBaseUrl());
        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
        final String originHost = originUrl.getHost();
        final Integer originPort = UrlUtils.getPort(originUrl.toString());
        final String destHost = destUrl.getHost();
        final Integer destPort = UrlUtils.getPort(clusterNodeHost);
///                        final String destNodeName = String.format("%s:%s", destHost, destPort);
        final String originNodeName = String.format("%s:%s", originHost, originPort);
        final String destUri = String.format("/wsv2/folib/%s", originNodeName);
        final boolean enableSSL = HttpUtil.isHttps(clusterNodeHost);
        //     String uri = "ws://" + destHost + ":" + destPort + destUri;
        String uri = String.format("%s://%s:%s", enableSSL ? "wss" : "ws", destHost, destPort + destUri);

        String targetHostName = getTargetHostName(nodeInfo);
        Session session1 = FOLIB_WS_RUN_MAP.get(targetHostName);
        if (!(session1 != null && session1.isOpen())) {
            try {
                connectToServer(targetHostName, uri);
            } catch (DeploymentException | IOException e) {
                log.error("connectToServer fail , retry...", e);
            }
        } else {
            Long l = sessionIdleMap.get(session1);
            if (l != null) {
                long idleTime = System.currentTimeMillis() - l;
                if (idleTime < 1000 * 20) {
                    return;
                }
                log.info("send ws HEARD_BEAT {}", targetHostName);
                try {
                    WSMessageResponse wsMessageResponse = sendRequest(targetHostName, new WSMessageRequest(Command.HEARD_BEAT));
                } catch (InterruptedException | ExecutionException | TimeoutException e) {
                    try {
                        session1.close();
                    } catch (IOException ex) {
                        log.error("close exception", e);
                    }
                    log.error("ping Exception", e);
                }
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


    public Session connectToServer(String targetHostName, String uri) throws DeploymentException, IOException {
        log.info("connect ws {}", uri);
        FolibWsClient folibWsClient = new FolibWsClient(targetHostName, uri);
        return ContainerProvider.getWebSocketContainer().connectToServer(folibWsClient, URI.create(uri));
    }

    public void registerSession(String targetHostName, Session session) {
        if (!session.isOpen()) {
            throw new IllegalStateException("registration of unopened sessions is not allowed");
        }
        log.info("registerSession [targetHostName:{} session:{}]", targetHostName, session);
        FOLIB_WS_RUN_MAP.put(targetHostName, session);
        sessionIdleMap.put(session, System.currentTimeMillis());
    }

    public Session unRegisterSession(String targetHostName) {
        Session session = FOLIB_WS_RUN_MAP.remove(targetHostName);
        sessionIdleMap.remove(session);
        log.info("unRegisterSession [targetHostName:{} session:{}]", targetHostName, session);
        if (session != null && session.isOpen()) {
            try {
                session.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        sessionLastSendTime.remove(session);
        sessionBytesSent.remove(session);
        sessionLocks.remove(session);
        return session;
    }

    public Session getSession(String targetHostName) {
        return FOLIB_WS_RUN_MAP.get(targetHostName);
    }

    private void sendBinary(String targetHostName, WSMessage wsMessageRequest) throws ExecutionException, InterruptedException, TimeoutException {
        Session session = getSession(targetHostName);
        if (session == null) {
            throw new RuntimeException("not found session with targetHostName:" + targetHostName);
        }
        if (!session.isOpen()) {
            throw new RuntimeException("session is closed , with targetHostName:" + targetHostName);
        }
        final long kbps = Optional.ofNullable(configurationManagementService.getConfiguration().getKbps()).orElse(0) * (1024L);

        final Collection<ClusterDispatchNodeDto> clusterDispatchNodeDtos = configurationManagementService.getMutableConfigurationClone().getClusterDispatchNode().values();
        final Map<String, Long> nodeKbpsMap = clusterDispatchNodeDtos.stream().collect(Collectors.toMap(e -> String.format("%s:%s", UrlUtils.getHost(e.getClusterNodeHost()), UrlUtils.getPort(e.getClusterNodeHost())), e -> null != e.getKbps() ? e.getKbps() * 1024L : 0L));
        final long finalKbps = Optional.ofNullable(nodeKbpsMap.get(targetHostName)).filter(k -> k > 0).orElse(kbps);

        sendBinary(session, wsMessageRequest, finalKbps);
    }

    private void sendBinary(Session session, WSMessage wsMessage, long finalKbps) throws ExecutionException, InterruptedException, TimeoutException {
        ByteBuffer byteBuffer = ByteBuffer.wrap(KryoSerializationUtil.serialize(wsMessage));
        sessionIdleMap.put(session, System.currentTimeMillis());
        try {
            sendBinary(session, byteBuffer, finalKbps);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        //session.getBasicRemote().sendBinary(byteBuffer);

    }

    public WSMessageResponse sendRequest(String targetHostName, WSMessageRequest wsMessageRequest) throws ExecutionException, InterruptedException, TimeoutException {
        return sendRequest(targetHostName, wsMessageRequest, 5);
    }

    public WSMessageResponse sendRequest(String targetHostName, WSMessageRequest wsMessageRequest, int timeout) throws ExecutionException, InterruptedException, TimeoutException {
        Session session = getSession(targetHostName);

        if (session == null) {
            throw new RuntimeException("not found session with targetHostName:" + targetHostName);
        }
        if (!session.isOpen()) {
            throw new RuntimeException("session is closed , with targetHostName:" + targetHostName);
        }
        CompletableFuture<WSMessageResponse> future = new CompletableFuture<>();
        REQUEST_FUTURES.put(wsMessageRequest.getId(), future);
        try {
            sendBinary(targetHostName, wsMessageRequest);
        } catch (Exception e) {
            log.error("sendBinary fail", e);
            future.completeExceptionally(e);
        }
        WSMessageResponse wsMessageResponse = future.get(timeout, TimeUnit.SECONDS);
        REQUEST_FUTURES.remove(wsMessageRequest.getId());
        return wsMessageResponse;
    }

    public void sendResponse(Session session, WSMessageResponse wsMessageResponse) throws ExecutionException, InterruptedException, TimeoutException {
        sendBinary(session, wsMessageResponse, 0L);
    }

    public CompletableFuture<WSMessageResponse> getFuture(String requestId) {
        return REQUEST_FUTURES.get(requestId);
    }

    public CompletableFuture<WSMessageResponse> releaseFuture(String requestId) {
        return REQUEST_FUTURES.remove(requestId);
    }


    private static final long _1_MB = 1024 * 1024; // 1MB
    private static final long DEFAULT_BYTES_PER_SECOND = _1_MB * 50; //缺省值50M
    private final Map<Session, Long> sessionLastSendTime = new ConcurrentHashMap<>();
    private final Map<Session, Long> sessionBytesSent = new ConcurrentHashMap<>();
    private final Map<Session, ReentrantLock> sessionLocks = new ConcurrentHashMap<>();

    private void sendBinary(Session session, ByteBuffer data, long finalKbps) throws IOException {
        String messageId = UUID.randomUUID().toString();
        //缺省填充
        if (finalKbps <= 0) {
            finalKbps = DEFAULT_BYTES_PER_SECOND;
        }

        long bytesToSend = data.remaining();
        long startTime = System.currentTimeMillis();
        log.info("sendBinary [size:{} , finalKbps:{} Kbps, messageId:{}]", bytesToSend, finalKbps, messageId);
        sessionLocks.putIfAbsent(session, new ReentrantLock(true));
        ReentrantLock lock = sessionLocks.get(session);
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
                int chunkSize = (int) Math.min(bytesToSend, availableBandwidth);
                // 准备数据包，包括协议头、消息ID和数据
                byte[] bytes = FOLIB_WS_PROTOCOL.getBytes();
                ByteBuffer chunk = ByteBuffer.allocate(chunkSize + bytes.length + messageId.getBytes().length + 1); // +1 for isLast flag
                chunk.put(bytes);
                chunk.put(messageId.getBytes());
                boolean isLast = bytesToSend == chunkSize; // 检查是否为最后一个片段
                chunk.put((byte) (isLast ? 1 : 0)); // isLast flag
                for (int i = 0; i < chunkSize; i++) {
                    chunk.put(data.get());
                }
                // 准备读取
                chunk.flip();
                // session.getBasicRemote().sendBinary(chunk);

                CompletableFuture<Void> completableFuture = new CompletableFuture<>();

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


                if (isLast) {
                    log.info("send success , time consuming:{}ms", System.currentTimeMillis() - startTime);
                }
                // 更新已发送的数据量
                sessionBytesSent.compute(session, (k, v) -> v + chunkSize);
                // 如果已经到达计算周期（1秒），重置会话状态
                if (elapsedTime >= 1000) {
                    sessionLastSendTime.put(session, currentTime);
                    sessionBytesSent.put(session, 0L);
                }
                // 减少待发送的数据量
                bytesToSend -= chunkSize;

            } finally {
                lock.unlock();
            }
        }
    }

}
