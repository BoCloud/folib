package com.folib.authentication.api.jwt;

import java.util.Map;

import javax.inject.Inject;

import com.folib.security.exceptions.ExpiredTokenException;
import com.folib.security.exceptions.InvalidTokenException;
import com.folib.users.security.JwtAuthenticationClaimsProvider.JwtAuthentication;
import com.folib.users.security.JwtClaimsProvider;
import com.folib.users.security.SecurityTokenProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.InternalAuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.authentication.dao.AbstractUserDetailsAuthenticationProvider;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

/**
 * @author @author veadan
 *
 */

public class JwtAuthenticationProvider extends AbstractUserDetailsAuthenticationProvider
{

    @Inject
    private UserDetailsService userDetailsService;

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    private JwtClaimsProvider jwtClaimsProvider;

    public JwtAuthenticationProvider(@JwtAuthentication JwtClaimsProvider jwtClaimsProvider)
    {
        this.jwtClaimsProvider = jwtClaimsProvider;
    }

    @Override
    protected void additionalAuthenticationChecks(UserDetails userDetails,
                                                  UsernamePasswordAuthenticationToken authentication)
        throws AuthenticationException
    {
        if (authentication.getCredentials() == null)
        {
            throw new BadCredentialsException("No credentials provided.");
        }

        String token = authentication.getCredentials().toString();

        Map<String, String> targetClaimMap = provideUserDetailsClaims(userDetails);
        try
        {
            securityTokenProvider.verifyToken(token, authentication.getPrincipal().toString(), targetClaimMap);
        }
        catch (ExpiredTokenException e)
        {
            throw new BadCredentialsException("expired");
        }
        catch (InvalidTokenException e)
        {
            logger.error(e.getMessage(), e);
            throw new BadCredentialsException("invalid.token");
        }

    }

    protected Map<String, String> provideUserDetailsClaims(UserDetails userDetails)
    {
        return jwtClaimsProvider.getClaims((SpringSecurityUser) userDetails);
    }

    @Override
    protected UserDetails retrieveUser(String username,
                                       UsernamePasswordAuthenticationToken authentication)
        throws AuthenticationException
    {
        UserDetails loadedUser;
        try
        {
            loadedUser = userDetailsService.loadUserByUsername(username);
        }
        catch (UsernameNotFoundException notFound)
        {
            throw notFound;
        }
        catch (Exception repositoryProblem)
        {
            throw new InternalAuthenticationServiceException(
                    repositoryProblem.getMessage(), repositoryProblem);
        }

        if (loadedUser == null)
        {
            throw new UsernameNotFoundException(String.format("User [%s] not found.", username));
        }
        return loadedUser;
    }

    @Override
    public boolean supports(Class<?> authentication)
    {
        //请注意是当前路径下的JwtAuthentication，不是JwtAuthenticationClaimsProvider.JwtAuthentication
        return com.folib.authentication.api.jwt.JwtAuthentication.class.isAssignableFrom(authentication);
    }

}
