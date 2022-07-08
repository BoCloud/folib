package com.veadan.folib.authentication.api.nuget;

import com.veadan.folib.authentication.api.jwt.JwtAuthenticationProvider;
import com.veadan.folib.users.security.JwtClaimsProvider;

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
