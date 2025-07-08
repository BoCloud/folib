package com.folib.users.userdetails;

import com.folib.domain.User;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import javax.inject.Inject;

public class DataBaseUserDetailService
        implements UserDetailsService
{

    private static final Logger logger = LoggerFactory.getLogger(DataBaseUserDetailService.class);

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private RelationalDatabaseUserService userService;

    @Override
    public UserDetails loadUserByUsername(String name)
            throws UsernameNotFoundException
    {
        logger.info("Loading user details for {}...", name);

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
