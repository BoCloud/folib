package com.veadan.folib.ws.client.handler.command;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.ws.client.manage.FolibWsClientRunManage;
import com.veadan.folib.ws.common.JsonEncoder;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 * @date 2023/11/14 01:18
 */
@Slf4j
@Component
public class FolibWsClientResCacheCommand implements FolibWsClientCommand<FolibWsClientResCacheCommand.Payload>{

    public static final String COMMAND = "/client/resCache";

    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(Payload payload) {
        final String cacheId = payload.getCacheId();
        final String resDataJson = payload.getResDataJson();
        final String resDataClass = payload.getResDataClassName();
        try {
            final Class<?> aClass = Class.forName(resDataClass);
            FolibWsClientRunManage.actionUpdateLockValue(cacheId, JSON.parseObject(resDataJson, aClass));
        } catch (ClassNotFoundException e) {
            log.error("缓存Ws服务端响应值失败", e);
        }
    }

    @Data
    @Accessors(chain = true)
    public static class Payload implements JsonEncoder
    {
        private String cacheId;
        private String resDataJson;
        private String resDataClassName;
    }
}
