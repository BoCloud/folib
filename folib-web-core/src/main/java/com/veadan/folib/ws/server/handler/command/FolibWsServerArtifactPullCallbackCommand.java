package com.veadan.folib.ws.server.handler.command;

import com.veadan.folib.model.request.ArtifactPromotionNodeOptionCallbackReq;
import com.veadan.folib.services.ArtifactPromotionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

/**
 * @author veadan
 * @date 2023/11/8 14:26
 */
@Component
@Slf4j
public class FolibWsServerArtifactPullCallbackCommand implements FolibWsServerCommand<ArtifactPromotionNodeOptionCallbackReq> {

    public static final String COMMAND = "/server/artifactPullCallback";


    @Inject
    private ArtifactPromotionService artifactPromotionService;
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(ArtifactPromotionNodeOptionCallbackReq model) {
        final String syncNo = model.getSyncNo();
        try {
            artifactPromotionService.artifactPullCallback(model);
            log.info("回调更新制品拉取（{}）成功", syncNo);
        } catch (Exception e) {
            log.error("删除节点（{}）失败", syncNo, e);
        }
    }
    
}
