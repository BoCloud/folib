package com.folib.controllers.logging;

import java.util.function.Function;

import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.boot.actuate.autoconfigure.logging.LogFileWebEndpointProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

/**
 * @author veadan
 */
@Configuration
@EnableConfigurationProperties(LogFileWebEndpointProperties.class)
public class LoggingConfiguration
{

    @Bean
    public Function<SseEmitter, SseEmitterAwareTailerListenerAdapter> tailerListenerAdapterPrototypeFactory()
    {
        return sseEmitter -> tailerListener(sseEmitter);
    }

    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    public SseEmitterAwareTailerListenerAdapter tailerListener(SseEmitter sseEmitter)
    {
        return new SseEmitterAwareTailerListenerAdapter(sseEmitter);
    }

}
