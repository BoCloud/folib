package com.veadan.folib.authentication;

import com.veadan.folib.config.UsersConfig;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * @author Przemyslaw Fusik
 */
@Configuration
@Import({ UsersConfig.class,
          AuthenticationConfig.class })
public class TestConfig
{

}
