package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.controllers.configuration.StoragesConfigurationController;
import com.veadan.folib.domain.DispatchStorageTree;
import com.veadan.folib.dto.ArtifactDispatchRepositoryDto;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.common.FolibWsAction;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.handler.command.FolibWsServerCommand;
import com.veadan.folib.ws.server.handler.command.FolibWsServerGetStoragesRepositoryTreeResCommand;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/10 00:38
 * @since x.x.x
 */
@Component
@Slf4j
public class FolibWsClientGetStoragesRepositoryTreeCommand implements FolibWsServerCommand<ArtifactDispatchRepositoryDto> {

    public static final String COMMAND = "/getStoragesRepositoryTreeCommand";
    
    @Autowired
    private StoragesConfigurationController storagesConfigurationController;
    
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(ArtifactDispatchRepositoryDto req) {
        try {
            final ResponseEntity<DispatchStorageTree> dispatchRepositories = storagesConfigurationController.getDispatchRepositories(req);
            final FolibWsClientContextInfo contextSessionInfo = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsClientContextInfo.class);
            final String syncId = contextSessionInfo.getSyncId();
            final FolibWsAction folibWsAction = new FolibWsAction()
                    .sync(syncId)
                    .command(FolibWsServerGetStoragesRepositoryTreeResCommand.COMMAND)
                    .payload(dispatchRepositories.getBody());
            contextSessionInfo.getWsRunInfo().doAction(folibWsAction);
//            contextSessionInfo.getWsRunInfo()
//                    .getSession()
//                    .sendMessage(new TextMessage(
//                            new FolibWsAction()
//                                    .sync(syncId)
//                                    .command(FolibWsServerGetStoragesRepositoryTreeResCommand.COMMAND)
//                                    .payload(dispatchRepositories.getBody())
//                                    .encode()));
        } catch (Exception e) {
            log.error("处理获取获取节点仓库Ws处理逻辑异常", e);
        }
    }
}
