package com.veadan.folib.ws.server;

import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.ArtifactPromotionService;
import org.springframework.stereotype.Component;

import javax.websocket.Session;

/**
 * @author pengYongQiang
 * @date 2024/2/13 16:32
 */
@Component
public class UploadCommandProcessor extends CommandProcessor{
    @Override
    public Command getCommand() {
        return Command.UPLOAD;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) {
        ArtifactSliceUploadReq date = (ArtifactSliceUploadReq) wsMessageRequest.getDate();
        SpringContextUtil.getBean(ArtifactPromotionService.class).sliceUpload(date);
        return "ok";
    }
}
