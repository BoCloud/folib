package com.veadan.folib.security.authentication.suppliers;

import com.veadan.folib.controllers.login.LoginController;
import com.veadan.folib.controllers.login.LoginInput;
import com.veadan.folib.authentication.api.password.PasswordAuthentication;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

/**
 * Works in conjunction {@link LoginController}
 *
 * @author Przemyslaw Fusik
 */
@Component
@Order(1)
public class JsonFormLoginSupplier implements AuthenticationSupplier
{

    private static final Logger logger = LoggerFactory.getLogger(JsonFormLoginSupplier.class);

    @Inject
    private ObjectMapper objectMapper;

    @Override
    public Authentication supply(@Nonnull HttpServletRequest request)
    {
        LoginInput loginInput = null;
        try
        {
            loginInput = objectMapper.readValue(request.getInputStream(), LoginInput.class);
        }
        catch (IOException e)
        {
            throw new BadCredentialsException("invalid.credentials");
        }

        return new PasswordAuthentication(loginInput.getUsername(), loginInput.getPassword());
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request)
    {
        return "POST".equalsIgnoreCase(request.getMethod()) &&
               request.getContentType()!=null &&
               request.getContentType().contains(MediaType.APPLICATION_JSON_VALUE) &&
               LoginController.REQUEST_MAPPING.equals(request.getRequestURI());
    }
}
