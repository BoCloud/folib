package com.veadan.folib.ws.server;

import com.veadan.folib.scanner.common.util.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.websocket.*;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import java.nio.ByteBuffer;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
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

        folibWsUtil.onOpen(nodeName, session);
    }

    @OnClose
    public void onClose(@PathParam("nodeName") String nodeName, Session session, CloseReason closeReason) {
        folibWsUtil.onClose(nodeName, session, closeReason);
    }

    @OnMessage
    public void onMessage(@PathParam("nodeName") String nodeName, ByteBuffer message, Session session) {
        folibWsUtil.onMessage(nodeName, message, session);
    }

    @OnError
    public void onError(@PathParam("nodeName") String nodeName, Session session, Throwable error) {
        folibWsUtil.onError(nodeName, session, error);
    }
}
