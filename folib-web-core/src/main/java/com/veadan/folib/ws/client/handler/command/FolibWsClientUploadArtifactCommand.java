package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.domain.PromotionNodeOption;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @author pengYongQiang
 * @date 2024/2/11 15:19
 */
@Slf4j
@Component
public class FolibWsClientUploadArtifactCommand  implements FolibWsClientCommand<PromotionNodeOption>{
    public static final String COMMAND = "/client/upload/artifact";
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(PromotionNodeOption promotionNodeOption) {
        System.out.println(123);
    }
}
