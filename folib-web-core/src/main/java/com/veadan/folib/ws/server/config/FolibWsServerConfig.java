package com.veadan.folib.ws.server.config;

import com.veadan.folib.ws.server.handler.FolibWsServerHandler;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;

/**
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/10/16 20:48
 * @since x.x.x
 */
@Configuration
@EnableWebSocket
public class FolibWsServerConfig implements WebSocketConfigurer
{
    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) 
    {
        registry.addHandler(new FolibWsServerHandler(), "");
    }
}
