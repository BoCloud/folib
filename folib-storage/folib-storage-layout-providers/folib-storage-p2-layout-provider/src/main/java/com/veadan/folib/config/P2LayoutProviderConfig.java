package com.veadan.folib.config;

import com.veadan.folib.providers.layout.P2LayoutProvider;
import com.veadan.folib.repository.P2RepositoryFeatures;
import com.veadan.folib.repository.P2RepositoryManagementStrategy;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({ "com.veadan.folib.repository",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage",
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
