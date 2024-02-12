package com.veadan.folib.ws.server;

import lombok.Data;

/**
 * @author pengYongQiang
 * @date 2024/2/11 18:18
 */
@Data
public class WSMessageResponse implements WSMessage {

    private String id;
    private Command command;
    private Object date;

    public WSMessageResponse(String id, Command command, Object date) {
        this.id = id;
        this.command = command;
        this.date = date;
    }
}
