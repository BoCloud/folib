package com.veadan.folib.ws.server;

import com.veadan.folib.promotion.KryoSerializationUtil;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.websocket.CloseReason;
import javax.websocket.Session;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.IllegalFormatFlagsException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.LinkedBlockingQueue;

import static com.veadan.folib.ws.common.FolibWsRunManageV2.FOLIB_WS_PROTOCOL;

/**
 * @author pengYongQiang
 * @date 2024/2/17 13:42
 */
@Slf4j
@Component
public class FolibWsUtil {

    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;
    //@Autowired
    //private PromotionTaskQueue promotionTaskQueue;
    @Autowired()
    private ThreadPoolTaskExecutor asyncWsCommandThreadPoolTaskExecutor;

    private static final ConcurrentHashMap<Session, ByteBuffer> messageBufferMap = new ConcurrentHashMap<>();
    private static final Map<Session, Map<String, ByteBuffer>> sessionMessageBufferMap = new ConcurrentHashMap<>();
    // 用于存储每个会话的消息片段
    private static final ConcurrentHashMap<Session, List<ByteBuffer>> messageFragmentsMap = new ConcurrentHashMap<>();


    public void onOpen(String targetHostName, Session session) {
        targetHostName = getTargetHostName(targetHostName);
        session.setMaxBinaryMessageBufferSize(1024 * 1024 * 1000);
        session.setMaxTextMessageBufferSize(1024 * 1024 * 1000);
        Session previousSession = folibWsRunManageV2.getSession(targetHostName);
        if (null != previousSession) {
            folibWsRunManageV2.unRegisterSession(targetHostName, targetHostName + " node already exists");
        }
        folibWsRunManageV2.registerSession(targetHostName, session);
        //promotionTaskQueue.registerPromotionTaskQueue(targetHostName);
        //promotionTaskQueue.registerV2PromotionTaskQueue(targetHostName);
    }

    public void onClose(String nodeId, Session session, CloseReason closeReason) {
        String msg = String.format("Connection closed successfully nodeId [%s] sessionId [%s] closeReason [%s]", nodeId, session.getId(), closeReason.toString());
        folibWsRunManageV2.cleanFuture(session, new RuntimeException(msg));
        folibWsRunManageV2.unRegisterSession(folibWsRunManageV2.getTargetNode(session), closeReason.toString());
        log.info(msg);
    }

    public void onError(String targetHostName, Session session, Throwable error) {
        if (session != null) {
            folibWsRunManageV2.cleanFuture(session, error);
        }
        log.error("WebSocket targetHostName [{}] error [{}]", targetHostName, error);
    }

    private final ConcurrentHashMap<String, LinkedBlockingQueue<ByteBuffer>> queueMap = new ConcurrentHashMap<>();

    //@Async("asyncWsCommandThreadPoolTaskExecutor")
    public void onMessage(String nodeName, ByteBuffer message, Session session) {
        String protocol = extractFolibWSProtocol(message);
        log.info("Received message nodeName [{}] current WS map [{}]", nodeName, folibWsRunManageV2.printWs());
        if (!FOLIB_WS_PROTOCOL.equals(protocol)) {
            throw new IllegalFormatFlagsException("Unknown protocol:" + protocol);
        }
        long messageSize = extractMessageSize(message);
        if (messageSize == 0) {
            throw new RuntimeException("Protocol exception messageSize");
        }
        String messageId = extractMessageId(message);
        if (StringUtils.isBlank(messageId)) {
            throw new RuntimeException("Protocol exception messageId");
        }

        /**
         * 这里不直接放到线程池处理，而是放到队列为了保证每个messageId的ByteBuffer在并发情况下都是有序的
         * 如果顺序不一致，会导致二进制反序列化出现错误。
         */
        LinkedBlockingQueue<ByteBuffer> queue1 = queueMap.computeIfAbsent(messageId, k -> {
            LinkedBlockingQueue<ByteBuffer> queue = new LinkedBlockingQueue<>();
            //asyncWsCommandThreadPoolTaskExecutor.execute(() -> {
            //    while (true) {
            //        ByteBuffer take = null;
            //        try {
            //            take = queue.take();
            //        } catch (Exception e) {
            //            throw new RuntimeException(e);
            //        }
            //
            //        boolean finish = consumerMsg(nodeName, take, session, messageId, messageSize);
            //        if (finish) {
            //            queue.clear();
            //            queueMap.remove(messageId);
            //            break;
            //        }
            //    }
            //});
            CompletableFuture.runAsync(() -> {
                while (true) {
                    ByteBuffer take = null;
                    try {
                        take = queue.take();
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }

                    boolean finish = consumerMsg(nodeName, take, session, messageId, messageSize);
                    if (finish) {
                        queue.clear();
                        queueMap.remove(messageId);
                        break;
                    }
                }
            }, asyncWsCommandThreadPoolTaskExecutor).exceptionally(e -> {
                log.error("Exception in consumerMsg: {}", e.getMessage(), e);
                throw new RuntimeException(e);
            });
            return queue;
        });
        log.debug("Copy ByteBuffer nodeName [{}] messageId [{}]", nodeName, messageId);
        ByteBuffer copy = ByteBuffer.allocate(message.remaining());
        copy.put(message);
        copy.flip();
        try {
            queue1.put(copy);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        log.debug("Copy ByteBuffer success nodeName [{}] messageId [{}] queueSize [{}]", nodeName, messageId, queue1.size());
    }

    private boolean consumerMsg(String nodeName, ByteBuffer message, Session session, String messageId, long messageSize) {
        // boolean isLast = extractLastFlag(message);
        /**
         * 当一个message一直没收到完整数据长度，之前的缓存不会释放，导致内存泄露
         * 但是一般不会出现，因为一般是session连接强制被中断导致收不到isLast标记，而session一旦中断，就会释放引用，回收对象
         */

        ByteBuffer completeMessage = sessionMessageBufferMap.computeIfAbsent(session, k -> new ConcurrentHashMap<>())
                .compute(messageId, (id, existingBuffer) -> {
                    if (existingBuffer == null) {
                        int initCapacity = 1024 * 1024;
                        if (messageSize < initCapacity) {
                            initCapacity = (int) messageSize;
                        }
                        // 第一次接收此ID的数据，直接使用传入的数据大小作为初始大小
                        return ByteBuffer.allocate(Math.max(message.remaining(), initCapacity)); // 至少分配1024字节
                    } else if (existingBuffer.remaining() < message.remaining()) {
                        // 现有缓冲区不足以存储新增数据，需要扩容
                        int newCapacity = existingBuffer.capacity() + message.remaining();
                        ByteBuffer newBuffer = ByteBuffer.allocate(newCapacity);
                        existingBuffer.flip(); // 切换为读模式
                        newBuffer.put(existingBuffer); // 复制现有数据到新缓冲区
                        return newBuffer;
                    } else {
                        // 现有缓冲区足够大，直接返回
                        return existingBuffer;
                    }
                });

        // 添加新接收的数据
        completeMessage.put(message);
        int position = completeMessage.position();
        log.info("NodeName [{}] messageId [{}] received [{}/{}]", nodeName, messageId, position, messageSize);
        if (position == messageSize) {

            // 最后一片数据，处理完整消息，切换为读模式
            completeMessage.flip();
            Object msgObj = null;
            try {
                msgObj = KryoSerializationUtil.deserialize(completeMessage.array());
                handleMessage(nodeName, session, msgObj);
            } catch (Exception e) {
                handleExceptionMessage(nodeName, session, msgObj, messageId, e);
            }
            // 清理资源
            sessionMessageBufferMap.get(session).remove(messageId);
        } else if (position > messageSize) {
            throw new IllegalStateException("Received data exceeds actual message size");
        } else {
            // 更新缓冲区以便接收更多数据
            sessionMessageBufferMap.get(session).put(messageId, completeMessage);
        }
        return position == messageSize;
    }

    private void handleExceptionMessage(String nodeName, Session session, Object msgObj, String messageId, Exception e) {
        log.error("HandleMessage exception nodeName [{}] sessionId [{}] messageId [{}] ", nodeName, session.getId(), messageId, e);
        if (Objects.isNull(msgObj) || !(msgObj instanceof WSMessage)) {
            throw new RuntimeException(String.format("HandleMessage exception nodeName [%s] sessionId [%s] messageId [%s] message content error or is null", nodeName, session.getId(), messageId));
        }
        WSMessage wsMessage = (WSMessage) msgObj;
        WSMessageResponse error = WSMessageResponse.error(wsMessage.getId(), null, e.getMessage());
        try {
            new RetryTask(3) {
                @Override
                protected void exec(RetryTask retryTask) throws Exception {
                    try {
                        Session wsSession =  session;
                        if (Objects.isNull(wsSession) || !wsSession.isOpen()) {
                            wsSession = folibWsRunManageV2.getSession(nodeName);
                        }
                        folibWsRunManageV2.sendResponse(nodeName, wsSession, error);
                    } catch (Exception ex) {
                        log.error("SendResponse exception messageId [{}] error [{}]", messageId, ex);
                        throw new RuntimeException(ex);
                    }
                }
            }.call();
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }

    private String extractFolibWSProtocol(ByteBuffer message) {
        byte[] messageIdBytes = new byte[FOLIB_WS_PROTOCOL.length()];
        message.get(messageIdBytes);
        return new String(messageIdBytes, StandardCharsets.UTF_8);
    }

    private long extractMessageSize(ByteBuffer message) {
        return message.getLong();
    }

    private String extractMessageId(ByteBuffer message) {
        // 消息ID固定长度，UUID
        byte[] messageIdBytes = new byte[36];
        message.get(messageIdBytes);
        return new String(messageIdBytes, StandardCharsets.UTF_8);
    }


    private void handleMessage(String nodeName, Session session, Object msgObj) {
        if (msgObj instanceof WSMessageResponse) {
            processWSMessageResponse(nodeName, (WSMessageResponse) msgObj, session);
        } else if (msgObj instanceof WSMessageRequest) {
            processWSMessageRequest(nodeName, (WSMessageRequest) msgObj, session);
        } else {
            throw new RuntimeException("unknown type :" + msgObj.getClass());
        }
    }

    private void processWSMessageResponse(String nodeName, WSMessageResponse response, Session session) {
        log.info("NodeName [{}] response [{}]", nodeName, response);
        String id = response.getId();
        CompletableFuture<WSMessageResponse> future = folibWsRunManageV2.getFuture(session, id);
        if (future == null) {
            log.warn("ID [{}] future is null", id);
            return;
        }
        future.complete(response);
    }

    private void processWSMessageRequest(String nodeName, WSMessageRequest msgObj, Session session) {
        log.info("NodeName [{}] Request [{}]", nodeName, msgObj);
        ObjectProvider<CommandProcessor> beanProvider = SpringContextUtil.getApplicationContext().getBeanProvider(CommandProcessor.class);
        for (CommandProcessor commandProcessor : beanProvider) {
            if (commandProcessor.getCommand().equals(msgObj.getCommand())) {
                commandProcessor.execute(nodeName, msgObj, session);
                return;
            }
        }
        throw new RuntimeException(String.format("Not found CommandProcessor with Command %s", msgObj.getCommand()));
    }

    private String getTargetHostName(String targetHostName) {
        if (StringUtils.isBlank(targetHostName)) {
            throw new RuntimeException("Param targetHostName cannot be empty");
        }
        String split = "_";
        if (targetHostName.contains(split)) {
            return targetHostName;
        }
        return targetHostName + FolibWsRunManageUtil.getEndpoint();
    }

}
