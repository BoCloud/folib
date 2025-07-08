package com.folib.config;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;
import java.security.Security;

/**
 * @author veadan
 * @date 2024/6/20
 **/
@Configuration
public class SecurityConfig {

    @PostConstruct
    public void init() {
        Security.addProvider(new BouncyCastleProvider());
    }
}
