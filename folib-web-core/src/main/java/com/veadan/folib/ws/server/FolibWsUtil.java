package com.veadan.folib.ws.server;

import com.veadan.folib.promotion.KryoSerializationUtil;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
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

    private static final ConcurrentHashMap<Session, ByteBuffer> messageBufferMap = new ConcurrentHashMap<>();
    private static final Map<Session, Map<String, ByteBuffer>> sessionMessageBufferMap = new ConcurrentHashMap<>();
    // 用于存储每个会话的消息片段
    private static final ConcurrentHashMap<Session, List<ByteBuffer>> messageFragmentsMap = new ConcurrentHashMap<>();


    public void onOpen(String targetHostName, Session session) {
        session.setMaxBinaryMessageBufferSize(1024 * 1024 * 1000);
        session.setMaxTextMessageBufferSize(1024 * 1024 * 1000);
        synchronized (targetHostName.intern()) {
            Session priviousSession = folibWsRunManageV2.getSession(targetHostName);
            if (null != priviousSession) {
                folibWsRunManageV2.unRegisterSession(targetHostName);
            }
            folibWsRunManageV2.registerSession(targetHostName, session);
        }
    }

    public void onClose(String nodeId, Session session, CloseReason closeReason) {
        log.info("连接关闭成功,nodeId:{} session_id:{} closeReason:{}", nodeId, session.getId(), closeReason.toString());
    }

    public void onError(String targetHostName, Session session, Throwable error) {
        log.error("WebSocket(nodeName = {})发生错误 ", targetHostName, error);
    }


    public void onMessageV4(String nodeName, ByteBuffer message, Session session) {
        String protocol = extractFolibWSProtocol(message);
        if (!FOLIB_WS_PROTOCOL.equals(protocol)) {
            throw new IllegalFormatFlagsException("unknown protocol:" + protocol);
        }
        String messageId = extractMessageId(message);
        boolean isLast = extractLastFlag(message);
        /**
         * 当一个messageId一直没收到isLast标记，之前的缓存不会释放，导致内存泄露
         * 但是一般不会出现，因为一般是session连接强制被中断导致收不到isLast标记，而session一旦中断，就会释放引用，回收对象
         */
        log.info("onMessageV3 messageId:{},isLast:{}", messageId, isLast);
        ByteBuffer completeMessage = sessionMessageBufferMap.computeIfAbsent(session, k -> new ConcurrentHashMap<>())
                .compute(messageId, (id, existingBuffer) -> {
                    if (existingBuffer == null) {
                        // 第一次接收此ID的数据，直接使用传入的数据大小作为初始大小
                        return ByteBuffer.allocate(Math.max(message.remaining(), 1024 * 1024)); // 至少分配1024字节
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

        if (isLast) {
            // 最后一片数据，处理完整消息
            completeMessage.flip(); // 切换为读模式
            handleMessage(nodeName, session, completeMessage);
            sessionMessageBufferMap.get(session).remove(messageId); // 清理资源
        } else {
            // 更新缓冲区以便接收更多数据
            sessionMessageBufferMap.get(session).put(messageId, completeMessage);
        }
    }

    private String extractFolibWSProtocol(ByteBuffer message) {
        byte[] messageIdBytes = new byte[FOLIB_WS_PROTOCOL.length()];
        message.get(messageIdBytes);
        return new String(messageIdBytes, StandardCharsets.UTF_8);
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
            processWSMessageResponse(nodeName,(WSMessageResponse) msgObj, session);
        } else if (msgObj instanceof WSMessageRequest) {
            processWSMessage((WSMessageRequest) msgObj, session);
        } else {
            throw new RuntimeException("unknown type :" + msgObj.getClass());
        }
    }

    private void processWSMessageResponse(String nodeName,WSMessageResponse response, Session session) {
        String id = response.getId();
        CompletableFuture<WSMessageResponse> future = folibWsRunManageV2.getFuture(id);
        future.complete(response);
    }

    private void processWSMessage(WSMessageRequest msgObj, Session session) {
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
