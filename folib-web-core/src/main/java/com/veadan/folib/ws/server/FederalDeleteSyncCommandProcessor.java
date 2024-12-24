package com.veadan.folib.ws.server;

import com.veadan.folib.domain.policy.FederalPromotionPolicyService;
import com.veadan.folib.domain.policy.dto.SyncArtifatDTO;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.websocket.Session;
import java.util.List;

@Slf4j
@Component
public class FederalDeleteSyncCommandProcessor extends CommandProcessor{

    @Override
    public Command getCommand() {
        return Command.FEDERAL_DELETE_SYNC;
    }


    @Override
    protected Object doExecute(WSMessageRequest wsMessageRequest, Session session) throws Exception {
        List<SyncArtifatDTO>  dtos = (List<SyncArtifatDTO>) wsMessageRequest.getDate();
        SpringContextUtil.getBean(FederalPromotionPolicyService.class).federalDeleteArtifatSync(dtos);
        return "ok";
    }
}
