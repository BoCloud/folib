package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.domain.DispatchStorageTree;
import com.veadan.folib.ws.client.context.FolibWsClientContextInfo;
import com.veadan.folib.ws.client.manage.FolibWsServerRunManage;
import com.veadan.folib.ws.common.FolibWsSessionContextHolder;
import com.veadan.folib.ws.common.JsonEncoder;
import com.veadan.folib.ws.server.context.FolibWsServerContextInfo;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsClientActionResCommand implements FolibWsClientCommand<ResponseEntity<DispatchStorageTree>> 
{
    public static final String COMMAND = "/actionResCommand";
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(ResponseEntity<DispatchStorageTree> res) 
    {
        final FolibWsClientContextInfo contextSessionInfo = FolibWsSessionContextHolder.getContextSessionInfo(FolibWsClientContextInfo.class);
        final String syncId = contextSessionInfo.getSyncId();
        FolibWsServerRunManage.actionUpdateLockValue(syncId, res);
        
    }
}
