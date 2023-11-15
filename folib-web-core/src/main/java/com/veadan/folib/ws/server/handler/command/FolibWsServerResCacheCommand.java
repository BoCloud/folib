package com.veadan.folib.ws.server.handler.command;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.ws.client.handler.command.FolibWsClientCommand;
import com.veadan.folib.ws.common.JsonEncoder;
import com.veadan.folib.ws.server.manage.FolibWsServerRunManage;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/11/14 01:18
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsServerResCacheCommand implements FolibWsClientCommand<FolibWsServerResCacheCommand.Payload> {

    public static final String COMMAND = "/server/resCache";

    
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
            FolibWsServerRunManage.actionUpdateLockValue(cacheId, JSON.parseObject(resDataJson, aClass));
        } catch (ClassNotFoundException e) {
            log.error("缓存Ws客户端响应值失败", e);
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
