package com.folib.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"com.folib.configuration",
                 "com.folib.io",
                 "com.folib.config",
                 "com.folib.providers",
                 "com.folib.resource",
                 "com.folib.services",
                 "com.folib.storage",
                 "com.folib.yaml"
               })
public class StorageCoreConfig
{

}
