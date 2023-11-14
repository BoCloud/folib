package com.veadan.folib.ws.server;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.client.handler.command.FolibWsClientConsoleCommand;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.context.FolibWsServerContextInfo;
import com.veadan.folib.ws.server.handler.dispatch.FolibWsServerCommandDispatch;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
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
public class FolibWsServer {
    @Autowired
    protected ConfigurationManager configurationManager;
//    @Autowired
//    private FolibWsServerCommandDispatch folibWsServerCommandDispatch;

    @Autowired
    private ThreadPoolTaskExecutor asyncWsCommandThreadPoolTaskExecutor;

    @OnOpen
    public void onOpen(@PathParam("nodeName") String nodeName, Session session) {
        try {
            final FolibWsServerRunManage.FolibWsClientRun wsClientRun = FolibWsServerRunManage.getWsClientRun(nodeName);
            if (null != wsClientRun) {
                final String baseUrl = configurationManager.getConfiguration().getBaseUrl();
                final String info = String.format("连接失败，当前节点（%s）已存在连接的客户端（%s）会话", baseUrl, nodeName);
                session.getBasicRemote().sendText(new FolibWsAction()
                        .command(FolibWsClientConsoleCommand.COMMAND)
                        .payload(new FolibWsClientConsoleCommand.Payload()
                                .setLevel(FolibWsClientConsoleCommand.LogConsoleLevel.ERROR)
                                .setContent(info))
                        .encode());
                log.info(info);
                session.close();
            }

            FolibWsServerRunManage.online(nodeName, session);
            log.info("连接建立成功，nodeName = {} session_id = {}", nodeName, session.getId());

            // 将连接的节点信息维护到数据库


        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @OnClose
    public void onClose(@PathParam("nodeName") String nodeName, Session session) {
        FolibWsServerRunManage.remove(nodeName);
        log.info("连接关闭成功，nodeName = {} session_id = {}", nodeName, session.getId());
    }

    @OnMessage
    public void onMessage(@PathParam("nodeName") String nodeName, String message, Session session) {
        asyncWsCommandThreadPoolTaskExecutor.submit(() -> {
            try {
                final FolibWsAction folibWsAction = JSON.parseObject(message, FolibWsAction.class);
                FolibWsSessionContextHolder.setContextSessionInfo(new FolibWsServerContextInfo()
                        .setNodeName(nodeName)
                        .setSyncId(folibWsAction.getSyncId())
                        .setWsRunInfo(FolibWsServerRunManage.findRunBySession(session)));
                FolibWsServerCommandDispatch.dispatch(folibWsAction);
            } catch (Exception e) {
                log.error("解析来自FolibWs客户端的消息（{}）失败", message, e);
            } finally {
                FolibWsSessionContextHolder.removeContextSessionInfo();
            }
    
            log.info("服务端收到客户端消息，nodeName = {}  {} message = {}", nodeName, message, session.getId());
        });
    }

    @OnError
    public void onError(@PathParam("nodeName") String nodeName, Session session, Throwable error) {
        log.error("WebSocket(nodeName = {})发生错误，错误信息为: {} ", nodeName, error.getMessage());
        error.printStackTrace();
    }
}
