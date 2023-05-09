package com.veadan.folib.authentication.api.password;

import java.util.Optional;

import javax.inject.Inject;

import com.veadan.folib.authentication.api.AuthenticationCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.DaoAuthenticationProvider;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;

/**
 * @author veadan
 * @author xuxinping
 */
public class PasswordAuthenticationProvider extends DaoAuthenticationProvider
{

    private static final Logger logger = LoggerFactory.getLogger(PasswordAuthenticationProvider.class);

    @Inject
    private AuthenticationCache authenticationCache;

    @Override
    @Inject
    public void setPasswordEncoder(PasswordEncoder passwordEncoder)
    {
        super.setPasswordEncoder(passwordEncoder);
    }

    @Override
    @Inject
    public void setUserDetailsService(UserDetailsService userDetailsService)
    {
        super.setUserDetailsService(userDetailsService);
    }

    @Override
    public boolean supports(Class<?> authentication)
    {
        return PasswordAuthentication.class.isAssignableFrom(authentication);
    }

    @Override
    public Authentication authenticate(Authentication authentication)
        throws AuthenticationException
    {
        try
        {
            return super.authenticate(authentication);
        }
        catch (BadCredentialsException e)
        {
            throw new BadCredentialsException("invalid.credentials");
        }
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication)
        throws AuthenticationException
    {
        UsernamePasswordAuthenticationToken cachedAuthentication = authenticationCache.getAuthenticationToken(userDetails.getUsername());

        if (Optional.ofNullable(cachedAuthentication)
                    .filter(c -> authentication.getCredentials() != null && c.getCredentials() != null)
                    .filter(c -> authenticationCache.matches(authentication.getCredentials()
                                                                           .toString(),
                                                             c.getCredentials()
                                                              .toString()))
                    .isPresent())

        {
            logger.info("Found cached authentication for [{}]", userDetails.getUsername());
            return;
        }

        try
        {
            super.additionalAuthenticationChecks(userDetails, authentication);
        }
        catch (BadCredentialsException e)
        {
            throw new BadCredentialsException("invalid.credentials");
        }

        authenticationCache.putAuthenticationToken(authentication);
    }

}
