package com.veadan.folib.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({ "com.veadan.folib.artifact",
                 "com.veadan.folib.configuration",
                 "com.veadan.folib.io",
                 "com.veadan.folib.config",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.resource",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage",
                 "com.veadan.folib.yaml"
               })
public class StorageCoreConfig
{

}
