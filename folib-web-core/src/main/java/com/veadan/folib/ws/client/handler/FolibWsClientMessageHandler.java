package com.veadan.folib.ws.client.handler;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.client.handler.dispatch.FolibWsClientCommandDispatch;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.BinaryWebSocketHandler;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:24
 * @since x.x.x
 */
@Slf4j
public class FolibWsClientMessageHandler extends BinaryWebSocketHandler {
    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) {
        final String textMessage = new String(message.asBytes());
        try {
            FolibWsSessionContextHolder.setSession(new FolibWsClientContextInfo().setSession(session));
            final FolibWsAction folibWsAction = JSON.parseObject(textMessage, FolibWsAction.class);
            FolibWsClientCommandDispatch.dispatch(folibWsAction);
        } catch (Exception e) {
            log.error("解析来自FolibWs服务端的消息（{}）失败", textMessage, e);
        } finally {
            FolibWsSessionContextHolder.remove();
        }
    }
}
