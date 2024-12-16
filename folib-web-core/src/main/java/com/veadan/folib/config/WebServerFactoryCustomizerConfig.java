package com.veadan.folib.config;

import org.eclipse.jetty.server.*;
import org.eclipse.jetty.server.handler.DefaultHandler;
import org.eclipse.jetty.server.handler.HandlerList;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.web.embedded.jetty.ConfigurableJettyWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Configuration;

/**
 * @author leipenghui
 **/
@Configuration
@ConditionalOnProperty(name = "server.ssl.enabled", havingValue = "true", matchIfMissing = false)
public class WebServerFactoryCustomizerConfig implements WebServerFactoryCustomizer<ConfigurableJettyWebServerFactory> {

    @Value("${server.port}")
    private int httpsPort;

    @Value("${http.port}")
    private int httpPort;

    @Override
    public void customize(ConfigurableJettyWebServerFactory factory) {
        factory.addServerCustomizers(
                server -> {
                    HttpConfiguration httpConfiguration = new HttpConfiguration();
                    httpConfiguration.setSecurePort(httpsPort);
                    httpConfiguration.setSecureScheme("https");
                    httpConfiguration.addCustomizer(new SecureRequestCustomizer());
                    ServerConnector connector = new ServerConnector(server);
                    connector.addConnectionFactory(new HttpConnectionFactory(httpConfiguration));
                    connector.setPort(httpPort);
                    server.addConnector(connector);
                    // Add a Handlers for requests
                    HandlerList handlers = new HandlerList();
                    handlers.addHandler(new SecuredRedirectCustomHandler());
                    for (Handler handler : server.getHandlers()) {
                        handlers.addHandler(handler);
                    }
                    // always last
                    handlers.addHandler(new DefaultHandler());
                    server.setHandler(handlers);
                }
        );
    }

}

