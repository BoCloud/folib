package com.veadan.folib.config;

import io.undertow.UndertowOptions;
import org.springframework.boot.web.embedded.undertow.UndertowDeploymentInfoCustomizer;
import org.springframework.boot.web.embedded.undertow.UndertowServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


@Configuration
public class UndertowConfig {

    @Bean
    public WebServerFactoryCustomizer<UndertowServletWebServerFactory> undertowCustomizer() {
        return factory -> factory.addBuilderCustomizers(builder -> {
            builder.setServerOption(io.undertow.UndertowOptions.ALLOW_ENCODED_SLASH, true);
            builder.setServerOption(io.undertow.UndertowOptions.DECODE_URL, true);

        });
    }
}
