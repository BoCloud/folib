package com.veadan.folib.web;

import org.springframework.web.filter.OncePerRequestFilter;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

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
        final String decodedRequestURI = URLEncoder.encode(request.getRequestURI(), StandardCharsets.UTF_8.name());
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
