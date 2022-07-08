package com.veadan.folib.security.authentication.suppliers;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;
import javax.servlet.http.HttpServletRequest;
import java.io.UnsupportedEncodingException;
import java.util.Base64;

import com.veadan.folib.authentication.api.password.PasswordAuthentication;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

/**
 * @author veadan
 */
@Component
@Order(4)
class BasicAuthenticationSupplier implements AuthenticationSupplier
{

    private static final Logger logger = LoggerFactory.getLogger(BasicAuthenticationSupplier.class);

    private String credentialsCharset = "UTF-8";

    @Override
    public boolean supports(HttpServletRequest request)
    {
        final String header = getAuthenticationHeaderValue(request);
        if (header == null || !header.startsWith("Basic "))
        {
            return false;
        }

        return true;
    }

    @CheckForNull
    @Override
    public Authentication supply(@Nonnull HttpServletRequest request)
    {
        String header = getAuthenticationHeaderValue(request);

        String[] tokens;
        try
        {
            tokens = extractAndDecodeHeader(header);
        }
        catch (UnsupportedEncodingException e)
        {
            throw new BadCredentialsException("Failed to decode basic authentication token", e);
        }

        String username = tokens[0];

        logger.debug("Basic Authentication Authorization header found for user '{}'", username);

        return new PasswordAuthentication(username, tokens[1]);
    }

    private String getAuthenticationHeaderValue(HttpServletRequest request)
    {
        return request.getHeader("Authorization");
    }

    public void setCredentialsCharset(String credentialsCharset)
    {
        Assert.hasText(credentialsCharset, "credentialsCharset cannot be null or empty");
        this.credentialsCharset = credentialsCharset;
    }

    /**
     * Decodes the header into a username and password.
     *
     * @throws BadCredentialsException
     *             if the Basic header is not present or is not valid
     *             Base64
     */
    private String[] extractAndDecodeHeader(String header)
        throws UnsupportedEncodingException
    {
        byte[] base64Token = header.substring(6)
                                   .getBytes(credentialsCharset);
        byte[] decoded;
        try
        {
            decoded = Base64.getDecoder().decode(base64Token);
        }
        catch (IllegalArgumentException e)
        {
            throw new BadCredentialsException("Failed to decode basic authentication token", e);
        }

        String token = new String(decoded, credentialsCharset);

        int delim = token.indexOf(':');

        if (delim == -1)
        {
            throw new BadCredentialsException("Invalid basic authentication token");
        }
        return new String[] { token.substring(0, delim),
                              token.substring(delim + 1) };
    }
}
