package com.veadan.folib.config.janusgraph;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan(basePackageClasses = CommonDbServerConfiguration.class)
public class CommonDbServerConfiguration
{

}
