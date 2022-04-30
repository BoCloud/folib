package com.veadan.folib.web;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;

import org.springframework.web.filter.OncePerRequestFilter;

/**
 * @author Przemyslaw Fusik
 */
public class DirectoryTraversalFilter
        extends OncePerRequestFilter
{

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain)
            throws ServletException, IOException
    {
        final String decodedRequestURI = URLDecoder.decode(request.getRequestURI(), StandardCharsets.UTF_8.name());
        URI requestURI;
        try
        {
            requestURI = new URI(decodedRequestURI);
        }
        catch (URISyntaxException e)
        {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST, "Invalid URI path provided.");
            return;
        }
        final URI normalizedURI = requestURI.normalize();
        if (!requestURI.equals(normalizedURI))
        {
            response.sendError(HttpServletResponse.SC_BAD_REQUEST,
                               "Invalid path provided. Please make sure there are no sequences like \"path/..\" in your request url.");
            return;
        }
        filterChain.doFilter(request, response);
    }

}
