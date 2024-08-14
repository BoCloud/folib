package com.veadan.folib.ws.server;

import com.veadan.folib.ws.common.FolibWsRunManageV2;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.websocket.Session;

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

    public void execute(String nodeName, WSMessageRequest wsMessageRequest, Session session) {
        WSMessageResponse wsMessageResponse = null;
        try {
            log.info("CommandProcessor.doExecute nodeName [{}] command [{}]", nodeName, getCommand());
            Object result = doExecute(wsMessageRequest, session);
            wsMessageResponse = WSMessageResponse.ok(wsMessageRequest.getId(), wsMessageRequest.getCommand(), result);
        } catch (Exception e) {
            log.error("commandProcessor nodeName [{}] execute exception [{}]", nodeName, e);
            wsMessageResponse = WSMessageResponse.error(wsMessageRequest.getId(), wsMessageRequest.getCommand(), e.getMessage());
        }
        try {
            log.info("CommandProcessor.sendResponse nodeName [{}] wsMessageResponse [{}]", nodeName, wsMessageResponse);
            folibWsRunManageV2.sendResponse(nodeName, session, wsMessageResponse);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    protected abstract Object doExecute(WSMessageRequest wsMessageRequest, Session session) throws Exception;
}
