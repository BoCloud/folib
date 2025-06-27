package com.veadan.folib.ws.server;

import com.veadan.folib.controllers.configuration.StoragesConfigurationController;
import com.veadan.folib.domain.DispatchStorageTree;
import com.veadan.folib.dto.ArtifactDispatchRepositoryDto;
import jakarta.websocket.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;


/**
 * @author veadan
 * @date 2024/2/13 16:50
 */
@Component
public class StoragesRepositoryTreeCommandProcessor extends CommandProcessor {
    @Autowired
    private StoragesConfigurationController storagesConfigurationController;

    @Override
    public Command getCommand() {
        return Command.STORAGES_REPOSITORY_TREE;
    }

    @Override
    public DispatchStorageTree doExecute(WSMessageRequest wsMessageRequest, Session session) {
        ArtifactDispatchRepositoryDto req = (ArtifactDispatchRepositoryDto) wsMessageRequest.getDate();
        final ResponseEntity<DispatchStorageTree> dispatchRepositories = storagesConfigurationController.getDispatchRepositories(req);
        return dispatchRepositories.getBody();
    }
}
