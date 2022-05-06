package com.veadan.folib.users.userdetails;

import javax.inject.Inject;

import com.veadan.folib.domain.User;
import com.veadan.folib.users.service.UserService;
import com.veadan.folib.users.service.impl.YamlUserService.Yaml;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

public class YamlUserDetailService
        implements UserDetailsService
{

    private static final Logger logger = LoggerFactory.getLogger(YamlUserDetailService.class);

    @Inject
    @Yaml
    private UserService userService;

    @Override
    public UserDetails loadUserByUsername(String name)
            throws UsernameNotFoundException
    {
        logger.debug("Loading user details for {}...", name);

        if (name == null)
        {
            throw new IllegalArgumentException("Username cannot be null.");
        }

        User user = userService.findByUsername(name);
        if (user == null)
        {
            logger.error("[authenticate] ERROR Cannot find user with the name {}", name);
            throw new UsernameNotFoundException("Cannot find user with that name");
        }

        UserDetails springUser = new FolibUserDetails(user);
        logger.info("Authorise under {}", springUser);

        return springUser;
    }

}
