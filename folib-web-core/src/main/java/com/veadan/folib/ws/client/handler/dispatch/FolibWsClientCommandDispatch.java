package com.veadan.folib.ws.client.handler.dispatch;

import com.veadan.folib.ws.common.FolibWsCommandDispatch;
import com.veadan.folib.ws.client.handler.command.FolibWsClientCommand;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @date 2023/10/19 19:40
 */
@Slf4j
@Component
public class FolibWsClientCommandDispatch extends FolibWsCommandDispatch<FolibWsClientCommand> 
{

    public FolibWsClientCommandDispatch(ApplicationContext applicationContext) {
        super(applicationContext);
    }

    @Override
    public Class<FolibWsClientCommand> getFolibWsCommandClass() {
        return FolibWsClientCommand.class;
    }
}
