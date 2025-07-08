package com.folib.authentication.api.nuget;

import com.folib.authentication.api.jwt.JwtAuthenticationProvider;
import com.folib.users.security.JwtClaimsProvider;

/**
 * @author @author veadan
 *
 */
public class SecurityTokenAuthenticationProvider extends JwtAuthenticationProvider
{
    
    public SecurityTokenAuthenticationProvider(JwtClaimsProvider jwtClaimsProvider)
    {
        super(jwtClaimsProvider);
    }

    @Override
    public boolean supports(Class<?> authentication)
    {
        return SecurityTokenAuthentication.class.isAssignableFrom(authentication);
    }

}
