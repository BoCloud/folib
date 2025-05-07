package com.veadan.folib.ws.server;

import cn.hutool.core.lang.UUID;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.services.ArtifactPromotionService;
import com.veadan.folib.ws.common.FolibWsRunManageUtil;
import com.veadan.folib.ws.common.FolibWsRunManageV2;
import jakarta.websocket.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;


import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

/**
 * @author pengYongQiang
 * @date 2024/2/24 20:32
 */
@Component
public class DelegateUploadCommandProcessor extends CommandProcessor {

    @Autowired
    private ArtifactPromotionService artifactPromotionService;

    @Autowired
    private FolibWsRunManageV2 folibWsRunManageV2;

    @Override
    public Command getCommand() {
        return Command.DELEGATE_UPLOAD;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) throws Exception {
        PromotionNodeOption promotionNodeOption = (PromotionNodeOption) wsMessageRequest.getDate();
        final String syncNo = String.format("SyncNo%s", UUID.randomUUID().toString(true));
        final String targetNode = folibWsRunManageV2.getTargetNode(FolibWsRunManageUtil.getTargetHostName(promotionNodeOption.getTargetPath()));
        promotionNodeOption.setTargetNode(targetNode);
        CompletableFuture<Void> voidCompletableFuture = artifactPromotionService.uploadArtifact(syncNo, promotionNodeOption, "localhost");
        voidCompletableFuture.get(1, TimeUnit.HOURS);
        return "ok";
    }
}
