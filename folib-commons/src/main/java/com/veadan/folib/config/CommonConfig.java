package com.veadan.folib.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

@Configuration
@ComponentScan({ "com.veadan.folib.booters",
                 "com.veadan.folib.configuration",
                 "com.veadan.folib.io",
                 "com.veadan.folib.net",
                 "com.veadan.folib.resource",
                 "com.veadan.folib.rest",
                 "com.veadan.folib.storage.repository",
                 "com.veadan.folib.url",
                 "com.veadan.folib.util",
                 "com.veadan.folib.yaml",
                 "com.veadan.folib.promotion",
                 "com.veadan.folib.cluster"})
@Import(PropertiesPathResolverConfig.class)
public class CommonConfig
{
}
