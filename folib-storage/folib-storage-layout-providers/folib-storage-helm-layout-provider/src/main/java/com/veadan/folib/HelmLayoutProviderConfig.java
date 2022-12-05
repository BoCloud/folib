package com.veadan.folib;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;


@Configuration
@ComponentScan({ "com.veadan.folib.repository",
                 "com.veadan.folib.providers",
                 "com.veadan.folib.services",
                 "com.veadan.folib.storage" })
public class HelmLayoutProviderConfig
{



}
