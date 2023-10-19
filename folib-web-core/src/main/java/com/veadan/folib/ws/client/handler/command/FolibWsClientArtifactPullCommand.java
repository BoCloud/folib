package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.domain.PromotionNodeOption;
import com.veadan.folib.services.ArtifactPromotionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Component
public class FolibWsClientArtifactPullCommand implements FolibWsClientCommand<PromotionNodeOption> 
{
    
    @Autowired
    private ArtifactPromotionService artifactPromotionService;
    
    @Override
    public String command() {
        return "/artifact/pull";
    }

    @Override
    public void execute(PromotionNodeOption promotionNodeOption) 
    { artifactPromotionService.nodeOption(promotionNodeOption, null); }
}
