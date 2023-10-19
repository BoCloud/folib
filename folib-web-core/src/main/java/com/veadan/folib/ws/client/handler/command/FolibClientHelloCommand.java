package com.veadan.folib.ws.client.handler.command;

import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:37
 * @since x.x.x
 */
@Component
public class FolibClientHelloCommand implements FolibClientCommand<String> {
    @Override
    public String command() {
        return "/hello";
    }

    @Override
    public void execute(String content) 
    {
        System.out.println("接收到hello command内容：" + content);
    }
}
