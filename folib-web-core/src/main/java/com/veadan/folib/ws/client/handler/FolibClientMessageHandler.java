package com.veadan.folib.ws.client.handler;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.ws.FolibWsAction;
import com.veadan.folib.ws.client.handler.dispatch.FolibClientCommandDispatch;
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
public class FolibClientMessageHandler extends BinaryWebSocketHandler
{
    private FolibClientCommandDispatch folibClientCommandDispatch;

    public FolibClientMessageHandler(FolibClientCommandDispatch folibClientCommandDispatch) {
        this.folibClientCommandDispatch = folibClientCommandDispatch;
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) 
    {
        final String textMessage = new String(message.asBytes());
        final FolibWsAction folibWsAction = JSON.parseObject(textMessage, FolibWsAction.class);
        folibClientCommandDispatch.dispatch(folibWsAction);
    }
}
