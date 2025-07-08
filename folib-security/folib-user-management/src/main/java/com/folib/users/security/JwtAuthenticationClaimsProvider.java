package com.folib.users.security;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.util.Collections;
import java.util.Map;

import javax.inject.Qualifier;

import com.folib.users.userdetails.SpringSecurityUser;
import org.springframework.stereotype.Component;

import static com.folib.users.security.JwtAuthenticationClaimsProvider.JwtAuthentication;

@Component
@JwtAuthentication
public class JwtAuthenticationClaimsProvider implements JwtClaimsProvider
{

    @Override
    public Map<String, String> getClaims(SpringSecurityUser user)
    {
        return Collections.singletonMap("userHash", String.valueOf(user.hashCode()));
    }

    @Qualifier
    @Retention(RUNTIME)
    public @interface JwtAuthentication {
        
    }
}
