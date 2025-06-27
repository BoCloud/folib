package com.veadan.folib.ws.common;

import cn.hutool.core.lang.UUID;
import cn.hutool.json.JSONUtil;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author veadan
 * @date 2023/10/19 16:27
 */
@Data
@Accessors(chain = true)
public class FolibWsAction implements JsonEncoder {
    private String syncId;
    private String command;
    private String payload;

    public String getSyncId() {
        return syncId;
    }

    public String getCommand() {
        return command;
    }

    public FolibWsAction command(String command) {
        this.command = command;
        return this;
    }

    public String getPayload() {
        return payload;
    }

    public FolibWsAction payload(Object object) {
        this.payload = JSONUtil.toJsonStr(object);
        return this;
    }

    public FolibWsAction sync() {
        this.syncId = UUID.fastUUID().toString(true);
        return this;
    }
    
    public FolibWsAction sync(String syncId) {
        this.syncId = syncId;
        return this;
    }
}
