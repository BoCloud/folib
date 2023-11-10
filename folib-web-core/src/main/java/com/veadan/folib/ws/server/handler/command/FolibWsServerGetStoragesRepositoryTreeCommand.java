package com.veadan.folib.ws.server.handler.command;

import com.veadan.folib.controllers.configuration.StoragesConfigurationController;
import com.veadan.folib.domain.DispatchStorageTree;
import com.veadan.folib.dto.ArtifactDispatchRepositoryDto;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.client.handler.command.FolibWsClientActionResCommand;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.TextMessage;

import java.io.IOException;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/10 00:38
 * @since x.x.x
 */
@Component
@Slf4j
public class FolibWsServerGetStoragesRepositoryTreeCommand implements FolibWsServerCommand<ArtifactDispatchRepositoryDto> {

    public static final String COMMAND = "/getStoragesRepositoryTreeCommand";
    
    @Autowired
    private StoragesConfigurationController storagesConfigurationController;
    
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(ArtifactDispatchRepositoryDto req) {
        final ResponseEntity<DispatchStorageTree> dispatchRepositories = storagesConfigurationController.getDispatchRepositories(req);
        final FolibWsClientContextInfo contextSessionInfo = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsClientContextInfo.class);
        try {
            final String syncId = contextSessionInfo.getSyncId();
            contextSessionInfo.getWsRunInfo()
                    .getSession()
                    .sendMessage(new TextMessage(
                            new FolibWsAction()
                                    .sync(syncId)
                                    .command(FolibWsClientActionResCommand.COMMAND)
                                    .payload(dispatchRepositories)
                                    .encode()));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
