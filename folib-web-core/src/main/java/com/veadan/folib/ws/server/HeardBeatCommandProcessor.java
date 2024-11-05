package com.veadan.folib.ws.server;

import org.springframework.stereotype.Component;

import javax.websocket.Session;

/**
 * @author pengYongQiang
 * @date 2024/2/13 16:50
 */
@Component
public class HeardBeatCommandProcessor extends CommandProcessor {

    @Override
    public Command getCommand() {
        return Command.HEARD_BEAT;
    }

    @Override
    public String doExecute(WSMessageRequest wsMessageRequest, Session session) {
        return "pong";
    }
}
