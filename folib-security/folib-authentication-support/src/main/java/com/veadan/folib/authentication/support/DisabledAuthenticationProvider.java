package com.veadan.folib.authentication.support;

import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

public class DisabledAuthenticationProvider implements AuthenticationProvider
{

    @Override
    public Authentication authenticate(Authentication authentication)
        throws AuthenticationException
    {
        return authentication;
    }

    @Override
    public boolean supports(Class<?> authentication)
    {
        return false;
    }

}
