package com.veadan.folib.ws.server;

import com.veadan.folib.scanner.common.util.SpringContextUtil;
import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;


import java.nio.ByteBuffer;

/**
 * @author veadan
 * @date 2023/10/19 15:18
 * @since x.x.x
 */
@Slf4j
@ServerEndpoint("/wsv2/folib/{nodeName}")
@Component
public class FolibWsServerV2 {
    FolibWsUtil folibWsUtil;

    public FolibWsServerV2() {
        folibWsUtil = SpringContextUtil.getBean(FolibWsUtil.class);
    }

    @OnOpen
    public void onOpen(@PathParam("nodeName") String nodeName, Session session) {
        log.info("NodeName [{}] session [{}]", nodeName, session.getId());
        folibWsUtil.onOpen(nodeName, session);
    }

    @OnClose
    public void onClose(@PathParam("nodeName") String nodeName, Session session, CloseReason closeReason) {
        log.info("NodeName [{}] session [{}]", nodeName, session.getId());
        folibWsUtil.onClose(nodeName, session, closeReason);
    }

    @OnMessage
    public void onMessage(@PathParam("nodeName") String nodeName, ByteBuffer message, Session session) {
        log.info("NodeName [{}] session [{}]", nodeName, session.getId());
        folibWsUtil.onMessage(nodeName, message, session);
    }

    @OnError
    public void onError(@PathParam("nodeName") String nodeName, Session session, Throwable error) {
        log.info("NodeName [{}] session [{}]", nodeName, session.getId());
        folibWsUtil.onError(nodeName, session, error);
    }
}
