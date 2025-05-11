package com.veadan.folib.configuration;

//import javax.enterprise.inject.Default;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;

@Configuration
@ComponentScan({ "com.veadan.folib.configuration",
                 "com.veadan.folib.security",
                 "com.veadan.folib.visitors" })
public class FolibSecurityConfig
{

    @Bean
    @Primary
    PasswordEncoder passwordEncoder()
    {
        return PasswordEncoderFactories.createDelegatingPasswordEncoder();
    }

}
