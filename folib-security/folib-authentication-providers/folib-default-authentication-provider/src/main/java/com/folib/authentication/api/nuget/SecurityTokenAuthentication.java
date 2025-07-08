package com.folib.authentication.api.nuget;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

/**
 * @author @author veadan
 *
 */
public class SecurityTokenAuthentication extends UsernamePasswordAuthenticationToken
{
    
    public SecurityTokenAuthentication(String principal,
                                       String credentials)
    {
        super(principal, credentials);
    }

    @Override
    public String getCredentials()
    {
        return (String) super.getCredentials();
    }

    @Override
    public String getPrincipal()
    {
        return (String) super.getPrincipal();
    }

    
}
