package com.folib.config;

import com.folib.providers.layout.P2LayoutProvider;
import com.folib.repository.P2RepositoryFeatures;
import com.folib.repository.P2RepositoryManagementStrategy;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({ "com.folib.repository",
                 "com.folib.providers",
                 "com.folib.services",
                 "com.folib.storage",
               })
public class P2LayoutProviderConfig
{

    @Bean(name = "p2LayoutProvider")
    P2LayoutProvider p2LayoutProvider()
    {
        return new P2LayoutProvider();
    }

    @Bean(name = "p2RepositoryFeatures")
    P2RepositoryFeatures p2RepositoryFeatures()
    {
        return new P2RepositoryFeatures();
    }

    @Bean(name = "p2RepositoryManagementStrategy")
    P2RepositoryManagementStrategy p2RepositoryManagementStrategy()
    {
        return new P2RepositoryManagementStrategy();
    }

}
