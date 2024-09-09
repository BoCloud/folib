package com.veadan.folib.ws.server;

import com.veadan.folib.scanner.common.util.SpringContextUtil;
import com.veadan.folib.services.UserSyncService;
import com.veadan.folib.users.dto.UserAuthDTO;
import com.veadan.folib.users.service.impl.RelationalDatabaseUserService;
import org.springframework.stereotype.Component;

import javax.websocket.Session;

/**
 * 用户权限同步
 * @author fengmaogen
 * @date 2024/2/13 16:32
 */
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
