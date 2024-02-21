package com.veadan.folib.ws.server;

import com.veadan.folib.scanner.common.util.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;

import javax.websocket.*;
import java.nio.ByteBuffer;

/**
 * @author pengYongQiang
 * @date 2024/2/12 23:09
 */
@ClientEndpoint
@Slf4j
public class FolibWsClient {
    private String uri;
    private String targetHostName;
    private FolibWsUtil folibWsUtil;

    public FolibWsClient(String targetHostName, String uri) {
        this.uri = uri;
        this.targetHostName = targetHostName;
        folibWsUtil = SpringContextUtil.getBean(FolibWsUtil.class);
    }

    @OnOpen
    public void onOpen(Session session) {
        folibWsUtil.onOpen(targetHostName, session);
    }

    @OnClose
    public void onClose(Session session, CloseReason closeReason) {
        folibWsUtil.onClose(targetHostName, session, closeReason);
    }

    @OnMessage
    public void onMessage(ByteBuffer message, Session session) {
        folibWsUtil.onMessageV4(targetHostName, message, session);
    }

    @OnError
    public void onError(Session session, Throwable error) {
        folibWsUtil.onError(targetHostName, session, error);
    }
}
