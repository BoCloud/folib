package com.folib.authentication.api.jwt;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

/**
 * @author @author veadan
 *
 */
public class JwtAuthentication extends UsernamePasswordAuthenticationToken
{

    public JwtAuthentication(String principal,
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
