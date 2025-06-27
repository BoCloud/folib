package com.veadan.folib.ws.client.handler;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.scanner.common.util.BeanUtils;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.client.handler.dispatch.FolibWsClientCommandDispatch;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.BinaryWebSocketHandler;

/**
 * @author veadan
 * @date 2023/10/19 16:24
 */
@Slf4j
public class FolibWsClientMessageHandler extends BinaryWebSocketHandler {

    private ThreadPoolTaskExecutor asyncWsCommandThreadPoolTaskExecutor;


    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        final String textMessage = new String(message.asBytes());
        this.getAsyncWsCommandThreadPoolTaskExecutor().submit(() -> {
            try {
                final FolibWsAction folibWsAction = JSON.parseObject(textMessage, FolibWsAction.class);
                FolibWsSessionContextHolder.setContextSessionInfo(new FolibWsClientContextInfo()
                        .setWsRunInfo(FolibWsClientRunManage.findRunBySession(session))
                        .setSyncId(folibWsAction.getSyncId()));
                FolibWsClientCommandDispatch.dispatch(folibWsAction);
            } catch (Exception e) {
                log.error("解析来自FolibWs服务端的消息（{}）失败", textMessage, e);
            } finally {
                FolibWsSessionContextHolder.removeContextSessionInfo();
            }
        });
    }

    private synchronized ThreadPoolTaskExecutor getAsyncWsCommandThreadPoolTaskExecutor() {
        if (null == this.asyncWsCommandThreadPoolTaskExecutor) {
            this.asyncWsCommandThreadPoolTaskExecutor = BeanUtils.getBean("asyncWsCommandThreadPoolTaskExecutor", ThreadPoolTaskExecutor.class);
        }
        return asyncWsCommandThreadPoolTaskExecutor;
    }
}
