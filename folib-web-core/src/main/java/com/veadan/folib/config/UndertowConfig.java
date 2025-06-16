package com.veadan.folib.config;

import io.micrometer.core.instrument.MeterRegistry;
import io.micrometer.core.instrument.binder.undertow.UndertowMetrics;
import io.undertow.server.handlers.MetricsHandler;
import org.springframework.boot.web.embedded.undertow.UndertowDeploymentInfoCustomizer;
import org.springframework.boot.web.embedded.undertow.UndertowServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;


@Configuration
public class UndertowConfig {

    @Bean
    public WebServerFactoryCustomizer<UndertowServletWebServerFactory> undertowCustomizer(MeterRegistry meterRegistry) {
        return factory -> {
            factory.addBuilderCustomizers(builder -> {
                builder.setServerOption(io.undertow.UndertowOptions.ALLOW_ENCODED_SLASH, true);
                builder.setServerOption(io.undertow.UndertowOptions.DECODE_URL, true);
            });

            factory.addDeploymentInfoCustomizers(deploymentInfo -> {
                // Micrometer MetricsHandler 注册
                deploymentInfo.addInitialHandlerChainWrapper(handler ->
                        new MetricsHandler(handler, new UndertowMetrics(meterRegistry))
                );
            });
        };
    }
}
