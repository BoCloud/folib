package com.veadan.folib.ws.server;

import com.veadan.folib.ws.server.manage.FolibWsClientRunManage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 15:18
 * @since x.x.x
 */
@Slf4j
@ServerEndpoint("/ws/folib/{nodeName}")
@Component
public class FolibWsServer 
{
    @OnOpen
    public void onOpen(@PathParam("nodeName") String nodeName, Session session) {
        FolibWsClientRunManage.online(nodeName, session);
        log.info("连接建立成功，nodeName = {} session_id = {}", nodeName, session.getId());
        try {
            session.getBasicRemote().sendText(String.format("%s 你已经成功上线！！！", nodeName));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @OnClose
    public void onClose(@PathParam("nodeName") String nodeName, Session session) {
        FolibWsClientRunManage.offline(nodeName);
        log.info("连接关闭成功，nodeName = {} session_id = {}", nodeName, session.getId());
    }

    @OnMessage
    public void onMessage(@PathParam("nodeName") String nodeName, String message, Session session) {
        log.info("服务端收到客户端消息，nodeName = {}  {} message = {}", nodeName, message, session.getId());
    }

    @OnError
    public void onError(@PathParam("nodeName") String nodeName, Session session, Throwable error) {
        log.error("WebSocket(nodeName = {})发生错误，错误信息为: {} ", nodeName, error.getMessage());
        error.printStackTrace();
    }
}
