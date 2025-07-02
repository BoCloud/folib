package com.veadan.folib.web;


import com.veadan.folib.util.UriUtils;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import org.springframework.web.filter.OncePerRequestFilter;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

/**
 * @author veadan
 */
public class DirectoryTraversalFilter
        extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain filterChain)
            throws ServletException, IOException {
        final String decodedRequestURI = UriUtils.encode(request.getRequestURI());
        URI requestURI;
        try {
            requestURI = new URI(decodedRequestURI);
        } catch (URISyntaxException e) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().write(String.format("Invalid URI path provided [%s].", decodedRequestURI));
            return;
        }
        final URI normalizedURI = requestURI.normalize();
        if (!requestURI.equals(normalizedURI)) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            response.getWriter().write(String.format("Invalid path provided [%s]. Please make sure there are no sequences like \"path/..\" in your request url.", decodedRequestURI));
            return;
        }
        filterChain.doFilter(request, response);
    }

}
