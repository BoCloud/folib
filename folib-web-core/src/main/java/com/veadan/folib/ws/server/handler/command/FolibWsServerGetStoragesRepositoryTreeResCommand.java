package com.veadan.folib.ws.server.handler.command;

import com.veadan.folib.domain.DispatchStorageTree;
import com.veadan.folib.ws.client.handler.command.FolibWsClientCommand;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.server.context.FolibWsServerContextInfo;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsServerGetStoragesRepositoryTreeResCommand implements FolibWsClientCommand<DispatchStorageTree> 
{
    public static final String COMMAND = "/server/getStoragesRepositoryTreeRes";
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(DispatchStorageTree res) 
    {
        final FolibWsServerContextInfo contextSessionInfo = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsServerContextInfo.class);
        final String syncId = contextSessionInfo.getSyncId();
        FolibWsClientRunManage.actionUpdateLockValue(syncId, res);
    }
}
