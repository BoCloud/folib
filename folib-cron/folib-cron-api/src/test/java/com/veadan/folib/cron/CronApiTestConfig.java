package com.veadan.folib.cron;

import com.veadan.folib.config.ClientConfig;
import com.veadan.folib.config.CommonConfig;
import com.veadan.folib.cron.config.CronTasksConfig;
import com.veadan.folib.providers.repository.RepositoryProvider;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.HashMap;
import java.util.Map;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import static com.veadan.folib.cron.CronApiTestConfig.TestConfig;

/**
 * @author Przemyslaw Fusik
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@ContextConfiguration(classes = { CommonConfig.class,
                                  ClientConfig.class,
                                  TestConfig.class,
                                  CronTasksConfig.class })
public @interface CronApiTestConfig
{

    @Configuration
    class TestConfig
    {

        @Bean
        Map<String, RepositoryProvider> repositoryProviders()
        {
            return new HashMap<>();
        }
    }
}
