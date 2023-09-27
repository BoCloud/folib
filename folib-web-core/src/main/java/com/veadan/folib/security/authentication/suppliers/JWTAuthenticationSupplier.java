package com.veadan.folib.security.authentication.suppliers;

import com.veadan.folib.authentication.api.jwt.JwtAuthentication;
import com.veadan.folib.security.authentication.JwtTokenFetcher;
import com.veadan.folib.security.exceptions.InvalidTokenException;
import com.veadan.folib.users.security.SecurityTokenProvider;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Optional;

import org.apache.commons.lang.StringUtils;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

@Component
@Order(1)
public class JWTAuthenticationSupplier
        implements AuthenticationSupplier, JwtTokenFetcher
{

    @Inject
    private SecurityTokenProvider securityTokenProvider;

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request)
    {
        final Optional<String> optToken = getToken(request);
        if (!optToken.isPresent())
        {
            return null;
        }

        final String token = optToken.get();
        String username;
        try
        {
            username = securityTokenProvider.getSubject(token);
        }
        catch (InvalidTokenException e)
        {
            throw new BadCredentialsException("invalid.token");
        }

        return new JwtAuthentication(username, token);
    }

    @Override
    public boolean supports(HttpServletRequest request)
    {
        boolean hasHeader = false;
        boolean hasCookie = false;

        // give priority to header based authentication, because it is more likely to be present
        if(request.getHeader(AUTHORIZATION_HEADER) != null)
        {
            String authHeader = request.getHeader(AUTHORIZATION_HEADER);
            hasHeader = StringUtils.isNotBlank(authHeader) && authHeader.startsWith(BEARER_AUTHORIZATION_PREFIX);
        }
        // fallback - check if a cookie is present (necessary for EventSource; check gh#1046).
        else if (request.getCookies() != null && matchesScope(request))
        {
            hasCookie = Arrays.stream(request.getCookies())
                              .anyMatch(c -> c.getName()
                                              .equals(AUTHORIZATION_COOKIE));
        }

        if (hasHeader || hasCookie)
        {
            return true;
        }

        return false;
    }

    private boolean matchesScope(HttpServletRequest request)
    {
        final String uri = request.getRequestURI();

        // wildcard match
        boolean matches = uri.startsWith("/api") || uri.startsWith("/storages");

        // exclude `/api/ping` since it's under the wild card match, but is called to check for liveliness which
        // means it might contain the `cookie` thus triggering a basic auth (gh#1687).
        if(matches && uri.equals("/api/ping")) {
            matches = false;
        }

        return matches;
    }

}
