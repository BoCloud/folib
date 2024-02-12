package com.veadan.folib.ws.server;

import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.websocket.Session;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

/**
 * @author pengYongQiang
 * @date 2024/2/13 16:30
 */
@Slf4j
@Component
public abstract class CommandProcessor {
    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;

    public abstract Command getCommand();

    public void execute(WSMessageRequest wsMessageRequest, Session session) {
        Object result = null;
        try {
            log.info("CommandProcessor.doExecute command:{}",getCommand());
            result = doExecute(wsMessageRequest, session);
        } catch (Exception e) {
            log.error("commandProcessor execute exception", e);
            result = e;

        }
        WSMessageResponse wsMessageResponse = wsMessageRequest.buildResponse(result);
        try {
            log.info("CommandProcessor.sendResponse wsMessageResponse:{}",wsMessageResponse);
            folibWsRunManageV2.sendResponse(session, wsMessageResponse);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            throw new RuntimeException(e);
        }
    }

    protected abstract Object doExecute(WSMessageRequest wsMessageRequest, Session session);
}
