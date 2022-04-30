package com.veadan.folib.authentication.impl.example;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

public class EmptyAuthenticationProvider implements AuthenticationProvider
{
    
    @Autowired(required = true)
    protected EmptyAuthenticationProviderComponent emptyAuthenticationProviderComponent;

    @Override
    public Authentication authenticate(Authentication authentication)
        throws AuthenticationException
    {
        return authentication;
    }

    @Override
    public boolean supports(Class<?> authentication)
    {
        return true;
    }

    
    
}
