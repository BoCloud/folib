package com.veadan.folib.security.authentication;

import com.veadan.folib.controllers.support.ErrorResponseEntityBody;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Optional;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.http.MediaType;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.AuthenticationEntryPoint;

public class Http401AuthenticationEntryPoint implements AuthenticationEntryPoint
{

    private static final String IS_AJAX_REQUEST_HEADER_NAME = "X-Requested-With";

    private static final String IS_AJAX_REQUEST_HEADER_VALUE = "XMLHttpRequest";

    private static final String FOLIB_REALM = "Strongbox Repository Manager";

    private static final String IS_REQUEST_OPTIONS = "options";

    @Inject
    private ObjectMapper objectMapper;
    
    @Override
    public void commence(HttpServletRequest request,
                         HttpServletResponse response,
                         AuthenticationException authException)
            throws IOException
    {
        String message = Optional.ofNullable(authException).map(e -> e.getMessage()).orElse("unauthorized");
        
        if (!IS_AJAX_REQUEST_HEADER_VALUE.equals(request.getHeader(IS_AJAX_REQUEST_HEADER_NAME)) &&
            !request.getMethod().equalsIgnoreCase(IS_REQUEST_OPTIONS))
        {
            response.setHeader("WWW-Authenticate", "Basic realm=\"" + FOLIB_REALM + "\"");
        }

        response.setContentType(MediaType.APPLICATION_JSON_VALUE);
        response.getWriter().println(objectMapper.writeValueAsString(new ErrorResponseEntityBody(message)));
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);      

        response.flushBuffer();
    }
    
}
