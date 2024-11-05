package com.veadan.folib.ws.server;

import lombok.Data;
import org.springframework.http.HttpStatus;

/**
 * @author pengYongQiang
 * @date 2024/2/11 18:18
 */
@Data
public class WSMessageResponse implements WSMessage {

    private String id;
    private Command command;
    private Object date;
    private HttpStatus status = HttpStatus.OK;

    public WSMessageResponse(String id, Command command, Object date) {
        this.id = id;
        this.command = command;
        this.date = date;
    }

    public WSMessageResponse(String id, Command command, Object date, HttpStatus status) {
        this.id = id;
        this.command = command;
        this.date = date;
        this.status = status;
    }

    public static WSMessageResponse ok(String id, Command command) {
        return ok(id, command, null);
    }

    public static WSMessageResponse ok(String id, Command command, Object date) {
        return new WSMessageResponse(id, command, date);
    }

    public static WSMessageResponse error(String id, Command command, Object date) {
        return new WSMessageResponse(id, command, date, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
