package com.veadan.folib.authentication.support;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

public class DisabledUserDetailsService implements UserDetailsService
{

    @Override
    public UserDetails loadUserByUsername(String username)
        throws UsernameNotFoundException
    {
        throw new UsernameNotFoundException(username);
    }

}
