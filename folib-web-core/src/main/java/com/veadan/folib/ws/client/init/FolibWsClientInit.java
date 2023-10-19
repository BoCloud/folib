package com.veadan.folib.ws.client.init;

import com.veadan.folib.ws.client.handler.dispatch.FolibWsClientCommandDispatch;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/19 16:25
 * @since x.x.x
 */
@Component
public class FolibWsClientInit implements ApplicationRunner 
{
    @Autowired
    private FolibWsClientCommandDispatch folibClientCommandDispatch;
    
    @Override
    public void run(ApplicationArguments args) throws Exception 
    {
        folibClientCommandDispatch.init();
    }
}
