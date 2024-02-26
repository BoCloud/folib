package com.veadan.folib.ws.server;

import cn.hutool.core.lang.UUID;
import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.services.ArtifactPromotionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.websocket.Session;
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

    @Override
    public Command getCommand() {
        return Command.DELEGATE_UPLOAD;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) throws Exception {
        PromotionNodeOption promotionNodeOption = (PromotionNodeOption) wsMessageRequest.getDate();
        final String syncNo = String.format("SyncNo%s", UUID.randomUUID().toString(true));
        CompletableFuture<Void> voidCompletableFuture = artifactPromotionService.uploadArtifact(syncNo, promotionNodeOption, "localhost");
        voidCompletableFuture.get(1, TimeUnit.HOURS);
        return "ok";
    }
}
