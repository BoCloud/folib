package com.veadan.folib.config;

import javax.servlet.ServletContext;

import com.veadan.folib.event.EventExecutorFactoryBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;

@Configuration
@ComponentScan({ "com.veadan.folib.event" })
@EnableAsync
public class EventsConfig
{

    @Bean
    public EventExecutorFactoryBean eventTaskExecutor(@Autowired(required = false) ServletContext servletContext)
    {
        return new EventExecutorFactoryBean(servletContext);
    }

}
