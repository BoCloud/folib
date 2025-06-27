package com.veadan.folib.ws.server;

import com.veadan.folib.domain.RepositoryInfo;
import com.veadan.folib.dto.DispatchRepoCheckDto;
import com.veadan.folib.services.ArtifactPromotionService;
import jakarta.websocket.Session;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;



/**
 * @author veadan
 * @date 2024/2/13 16:32
 */
@Component
@Slf4j
public class CheckTargetNodeRepositoryCommandProcessor extends CommandProcessor {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @Override
    public Command getCommand() {
        return Command.CHECK_TARGET_NODE_REPOSITORY;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) {
        RepositoryInfo repositoryInfo = (RepositoryInfo) wsMessageRequest.getDate();
        artifactPromotionService.validateStorageAndRepository(repositoryInfo.getStorageId(), repositoryInfo.getRepositoryId());
        return "ok";
    }
}
