package com.veadan.folib.ws.server;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.UserSyncService;
import com.veadan.folib.users.dto.UserAuthDTO;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import jakarta.websocket.Session;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;



/**
 * 用户权限同步
 * @author veadan
 * @date 2024/2/13 16:32
 */
@Slf4j
@Component
public class UserAuthSyncCommandProcessor extends CommandProcessor{
    @Override
    public Command getCommand() {
        return Command.USER_AUTH_SYNC;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) {
        UserAuthDTO date = (UserAuthDTO) wsMessageRequest.getDate();
        SpringContextUtil.getBean(UserSyncService.class).syncUserAuth(date);
        return "ok";
    }
}
