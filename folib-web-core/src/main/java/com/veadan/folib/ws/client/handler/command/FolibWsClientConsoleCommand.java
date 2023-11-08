package com.veadan.folib.ws.client.handler.command;

import com.veadan.folib.ws.common.JsonEncoder;
import lombok.Data;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Slf4j
@Component
public class FolibWsClientConsoleCommand implements FolibWsClientCommand<FolibWsClientConsoleCommand.Payload> 
{
    public static final String COMMAND = "/console";
    
    @Override
    public String command() {
        return COMMAND;
    }

    @Override
    public void execute(FolibWsClientConsoleCommand.Payload payload) 
    {
        final Integer level = payload.getLevel();
        final String content = payload.getContent();
        
        if (LogConsoleLevel.INFO.equals(level))
        { log.info(content); } 
        else if (LogConsoleLevel.WARN.equals(level))
        { log.warn(content); }
        else if (LogConsoleLevel.ERROR.equals(level))
        { log.error(content); }
        else
        { log.info("未知日志输出级别：{}", content); }
    }
    
    @Data
    @Accessors(chain = true)
    public static class Payload implements JsonEncoder
    {
        private Integer level;
        private String content;
    }
    
    public static class LogConsoleLevel
    {
        public static final Integer INFO = 1;
        public static final Integer WARN = 2;
        public static final Integer ERROR = 3;
    }
}
