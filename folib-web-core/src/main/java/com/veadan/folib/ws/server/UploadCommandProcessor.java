package com.veadan.folib.ws.server;

import com.veadan.folib.model.request.ArtifactSliceUploadReq;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.ArtifactPromotionService;
import jakarta.websocket.Session;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;


/**
 * @author pengYongQiang
 * @date 2024/2/13 16:32
 */
@Slf4j
@Component
public class UploadCommandProcessor extends CommandProcessor{
    @Override
    public Command getCommand() {
        return Command.UPLOAD;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) {
        ArtifactSliceUploadReq date = (ArtifactSliceUploadReq) wsMessageRequest.getDate();
        log.info("UploadCommand storageId [{}] repositoryId [{}] path [{}] chunkIndex [{}] chunkIndexMax [{}] originFileMd5 [{}]", date.getStorageId(), date.getRepositoryId(), date.getPath(), date.getChunkIndex(), date.getChunkIndexMax(), date.getOriginFileMd5());
        SpringContextUtil.getBean(ArtifactPromotionService.class).sliceUpload(date);
        return "ok";
    }
}
