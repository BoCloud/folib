package com.veadan.folib.ws.server;

import jakarta.websocket.Session;
import org.springframework.stereotype.Component;



/**
 * @author veadan
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
