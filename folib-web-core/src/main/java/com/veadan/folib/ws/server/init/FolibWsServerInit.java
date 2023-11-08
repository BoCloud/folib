package com.veadan.folib.ws.server.init;

import com.veadan.folib.ws.server.handler.dispatch.FolibWsServerCommandDispatch;
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
public class FolibWsServerInit implements ApplicationRunner {
    @Autowired
    private FolibWsServerCommandDispatch folibWsServerCommandDispatch;
    
    @Override
    public void run(ApplicationArguments args) throws Exception 
    {
        folibWsServerCommandDispatch.init();
    }
}
