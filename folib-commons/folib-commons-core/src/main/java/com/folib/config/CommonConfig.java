package com.folib.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

@Configuration
@ComponentScan({ "com.folib.booters",
                 "com.folib.configuration",
                 "com.folib.io",
                 "com.folib.net",
                 "com.folib.resource",
                 "com.folib.rest",
                 "com.folib.storage.repository",
                 "com.folib.url",
                 "com.folib.util",
                 "com.folib.yaml",
                 "com.folib.promotion",
                 "com.folib.cluster"})
@Import(PropertiesPathResolverConfig.class)
public class CommonConfig
{
}
