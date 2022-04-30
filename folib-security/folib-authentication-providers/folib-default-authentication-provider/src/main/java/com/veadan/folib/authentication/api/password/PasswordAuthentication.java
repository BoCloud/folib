package com.veadan.folib.authentication.api.password;

import java.util.Collection;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;

/**
 * @author Sergey Bespalov
 *
 */
public class PasswordAuthentication extends UsernamePasswordAuthenticationToken
{

    public PasswordAuthentication(Object principal,
                                  Object credentials)
    {
        super(principal, credentials);
    }

    public PasswordAuthentication(Object principal,
                                  Object credentials,
                                  Collection<? extends GrantedAuthority> authorities)
    {
        super(principal, credentials, authorities);
    }
    
}
