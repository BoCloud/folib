package com.veadan.folib.config;

import com.veadan.folib.testing.AssignedPorts;
import com.veadan.folib.yaml.YAMLMapperFactory;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * TestingCoreConfig.
 */
@Configuration
public class TestingCoreConfig
{

    @Bean(name = "assignedPorts")
    protected AssignedPorts assignedPorts()
    {
        return new AssignedPorts();
    }

    @Bean
    protected YAMLMapperFactory yamlMapperFactory()
    {
        return contextClasses -> new TestingYamlMapper(contextClasses);
    }

}
