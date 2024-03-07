package com.veadan.folib.ws.server;

import com.veadan.folib.promotion.KryoSerializationUtil;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
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
    @Autowired
    private PromotionTaskQueue promotionTaskQueue;
    @Autowired()
    private ThreadPoolTaskExecutor asyncWsCommandThreadPoolTaskExecutor;

    private static final ConcurrentHashMap<Session, ByteBuffer> messageBufferMap = new ConcurrentHashMap<>();
    private static final Map<Session, Map<String, ByteBuffer>> sessionMessageBufferMap = new ConcurrentHashMap<>();
    // 用于存储每个会话的消息片段
    private static final ConcurrentHashMap<Session, List<ByteBuffer>> messageFragmentsMap = new ConcurrentHashMap<>();


    public void onOpen(String targetHostName, Session session) {
        session.setMaxBinaryMessageBufferSize(1024 * 1024 * 1000);
        session.setMaxTextMessageBufferSize(1024 * 1024 * 1000);
        Session priviousSession = folibWsRunManageV2.getSession(targetHostName);
        if (null != priviousSession) {
            folibWsRunManageV2.unRegisterSession(targetHostName,targetHostName+" node already exists");
        }
        folibWsRunManageV2.registerSession(targetHostName, session);
        promotionTaskQueue.registerPromotionTaskQueue(targetHostName);
    }

    public void onClose(String nodeId, Session session, CloseReason closeReason) {
        folibWsRunManageV2.cleanFuture(session);
        log.info("连接关闭成功,nodeId:{} session_id:{} closeReason:{}", nodeId, session.getId(), closeReason.toString());
    }

    public void onError(String targetHostName, Session session, Throwable error) {
        folibWsRunManageV2.cleanFuture(session);
        log.error("WebSocket(nodeName = {})发生错误 ", targetHostName, error);
    }

    private final ConcurrentHashMap<String, LinkedBlockingQueue<ByteBuffer>> queueMap = new ConcurrentHashMap<>();

    //@Async("asyncWsCommandThreadPoolTaskExecutor")
    public void onMessageV4(String nodeName, ByteBuffer message, Session session) {
        String protocol = extractFolibWSProtocol(message);
        if (!FOLIB_WS_PROTOCOL.equals(protocol)) {
            throw new IllegalFormatFlagsException("unknown protocol:" + protocol);
        }
        long messageSize = extractMessageSize(message);
        if (messageSize == 0) {
            throw new RuntimeException("protocol Exception ,messageSize");
        }
        String messageId = extractMessageId(message);
        if (StringUtils.isBlank(messageId)) {
            throw new RuntimeException("protocol Exception ,messageId");
        }

        LinkedBlockingQueue<ByteBuffer> queue1 = queueMap.computeIfAbsent(messageId, k -> {
            LinkedBlockingQueue<ByteBuffer> queue = new LinkedBlockingQueue<>();
            asyncWsCommandThreadPoolTaskExecutor.execute(() -> {
                while (true){
                    ByteBuffer take = null;
                    try {
                        take = queue.take();
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }

                    boolean finish = consumerMsg(nodeName, take, session, messageId, messageSize);
                    if (finish){
                        queue.clear();
                        queueMap.remove(messageId);
                        break;
                    }
                }
            });
            return queue;
        });
        log.info("copy ByteBuffer, messageId:{}",messageId);
        ByteBuffer copy = ByteBuffer.allocate(message.remaining());
        copy.put(message);
        copy.flip();
        try {
            queue1.put(copy);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        log.info("copy ByteBuffer success, messageId:{},queueSize:{}",messageId,queue1.size());
    }

    private boolean consumerMsg(String nodeName, ByteBuffer message, Session session, String messageId, long messageSize) {
        // boolean isLast = extractLastFlag(message);
        /**
         * 当一个messageId一直没收到isLast标记，之前的缓存不会释放，导致内存泄露
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

        completeMessage.put(message); // 添加新接收的数据
        int capacity = completeMessage.capacity();
        log.info("onMessageV3 messageId:{},received:{}/{}", messageId, capacity, messageSize);
        if (capacity == messageSize) {

                // 最后一片数据，处理完整消息
                completeMessage.flip(); // 切换为读模式
                try {
                    handleMessage(nodeName, session, completeMessage);
                } catch (Exception e) {
                    handleExceptionMessage(session, e, messageId);
                }

            sessionMessageBufferMap.get(session).remove(messageId); // 清理资源
        } else {
            // 更新缓冲区以便接收更多数据
            sessionMessageBufferMap.get(session).put(messageId, completeMessage);
        }
        return capacity == messageSize;
    }

    private void handleExceptionMessage(Session session, Exception e, String messageId) {
        log.error("handleMessage Exception", e);
        WSMessageResponse error = WSMessageResponse.error(messageId, null, e.getMessage());
        try {
            new RetryTask(3) {
                @Override
                protected void exec(RetryTask retryTask) throws Exception {
                    try {
                        folibWsRunManageV2.sendResponse(session, error);
                    } catch (Exception ex) {
                        log.error("sendResponse exception , messageId: {}", messageId, ex);
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

    private boolean extractLastFlag(ByteBuffer message) {
        byte lastFlag = message.get();
        return lastFlag == 1;
    }

    private void handleMessage(String nodeName, Session session, ByteBuffer message) {
        Object msgObj = KryoSerializationUtil.deserialize(message.array());
        if (msgObj instanceof WSMessageResponse) {
            processWSMessageResponse(nodeName, (WSMessageResponse) msgObj, session);
        } else if (msgObj instanceof WSMessageRequest) {
            processWSMessageRequest((WSMessageRequest) msgObj, session);
        } else {
            throw new RuntimeException("unknown type :" + msgObj.getClass());
        }
    }

    private void processWSMessageResponse(String nodeName, WSMessageResponse response, Session session) {
        log.info("response {}", response);
        String id = response.getId();
        CompletableFuture<WSMessageResponse> future = folibWsRunManageV2.getFuture(session,id);
        if (future == null) {
            log.warn("id {} future is null", id);
            return;
        }
        future.complete(response);
    }

    private void processWSMessageRequest(WSMessageRequest msgObj, Session session) {
        log.info("request {}", msgObj);
        ObjectProvider<CommandProcessor> beanProvider = SpringContextUtil.getApplicationContext().getBeanProvider(CommandProcessor.class);
        for (CommandProcessor commandProcessor : beanProvider) {
            if (commandProcessor.getCommand().equals(msgObj.getCommand())) {
                commandProcessor.execute(msgObj, session);
                return;
            }
        }
        throw new RuntimeException(String.format("not found CommandProcessor with Command %s", msgObj.getCommand()));
    }

}
