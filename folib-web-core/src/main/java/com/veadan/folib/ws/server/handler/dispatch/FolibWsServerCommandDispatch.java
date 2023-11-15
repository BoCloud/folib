package com.veadan.folib.ws.server.handler.dispatch;

import com.veadan.folib.ws.common.FolibWsCommandDispatch;
import com.veadan.folib.ws.server.handler.command.FolibWsServerCommand;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 19:40
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsServerCommandDispatch extends FolibWsCommandDispatch<FolibWsServerCommand> 
{

    public FolibWsServerCommandDispatch(ApplicationContext applicationContext) {
        super(applicationContext);
    }

    @Override
    public Class<FolibWsServerCommand> getFolibWsCommandClass() {
        return FolibWsServerCommand.class;
    }
}
