package com.folib.config;

import com.folib.users.UsersFileManager;
import com.folib.users.dto.UsersDto;
import com.folib.users.service.impl.YamlUserService;
import com.folib.users.service.impl.YamlUserService.Yaml;

import java.io.IOException;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Spring configuration for all user-related code.
 *
 * @author 
 * @author veadan
 */
@Configuration
@ComponentScan({ "com.folib.users" })
@Import({ DataServiceConfig.class,
          CommonConfig.class,
          UsersAuthorizationConfig.class,
          UsersSecurityConfig.class })
public class UsersConfig
{

    @Inject
    @Yaml
    private YamlUserService userService;

    @Inject
    private UsersFileManager usersFileManager;

    @PostConstruct
    void init() throws IOException
    {
        final UsersDto securityUsers = usersFileManager.read();
        userService.setUsers(securityUsers);
    }


}
