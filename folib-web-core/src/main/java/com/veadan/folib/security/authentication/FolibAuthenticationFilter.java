package com.veadan.folib.security.authentication;

import java.io.IOException;
import java.util.Optional;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.veadan.folib.security.authentication.suppliers.AuthenticationSuppliers;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * @author Przemyslaw Fusik
 */
public class FolibAuthenticationFilter
        extends OncePerRequestFilter
{

    private static final Logger logger = LoggerFactory.getLogger(FolibAuthenticationFilter.class);

    private final AuthenticationManager authenticationManager;

    private final AuthenticationSuppliers authenticationSuppliers;

    public FolibAuthenticationFilter(AuthenticationSuppliers authenticationSuppliers,
                                     AuthenticationManager authenticationManager)
    {
        super();
        this.authenticationSuppliers = authenticationSuppliers;
        this.authenticationManager = authenticationManager;
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain)
        throws ServletException,
        IOException
    {
        Authentication authentication = authenticationSuppliers.supply(request);
        if (authentication == null)
        {
            authentication = SecurityContextHolder.getContext().getAuthentication();
            logger.debug("Authentication not supplied by any authentication supplier, using [{}] context authentication.",
                         Optional.ofNullable(authentication).map(a -> a.getClass().getSimpleName()).orElse("empty"));
        }
        else
        {
            logger.debug("Supplied [{}] authentication.", authentication.getClass().getSimpleName());
        }

        authentication = provideAuthentication(authentication);
        SecurityContextHolder.getContext().setAuthentication(authentication);

        filterChain.doFilter(request, response);
    }

    private Authentication provideAuthentication(Authentication authentication)
    {
        String authenticationName = Optional.ofNullable(authentication)
                                            .map(a -> a.getClass().getSimpleName())
                                            .orElse("empty");
        if (authentication == null || authentication.isAuthenticated())
        {
            logger.debug("Authentication {} already authenticated or empty, skip providers.", authenticationName);

            return authentication;
        }

        Authentication authResult = authenticationManager.authenticate(authentication);
        logger.debug("Authenticated with {}", authenticationName);

        return authResult;
    }
}
