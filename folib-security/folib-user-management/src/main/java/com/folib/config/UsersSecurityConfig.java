package com.folib.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({ "com.folib.users.security" })
public class UsersSecurityConfig
{

}
