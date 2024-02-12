package com.veadan.folib.ws.server;

import lombok.Data;

import java.util.UUID;

/**
 * @author pengYongQiang
 * @date 2024/2/11 18:18
 */
@Data
public class WSMessageRequest implements WSMessage {

    private String id;
    private Command command;
    private Object date;

    public WSMessageRequest(Command command, Object date) {
        this.command = command;
        this.date = date;
        this.id = UUID.randomUUID().toString();
    }

    public WSMessageRequest(Command command) {
        this(command, null);
    }

    public WSMessageResponse buildResponse(Object responseData) {
        return new WSMessageResponse(id, command, responseData);
    }
}
