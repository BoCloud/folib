package com.folib.security.authentication.suppliers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.authentication.api.password.PasswordAuthentication;
import com.folib.controllers.login.LoginController;
import com.folib.controllers.login.LoginInput;
import com.folib.util.RSAUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import javax.inject.Inject;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;

/**
 * Works in conjunction {@link LoginController}
 *
 * @author veadan
 */
@Component
@Order(3)
public class JsonFormLoginSupplier implements AuthenticationSupplier {

    private static final Logger logger = LoggerFactory.getLogger(JsonFormLoginSupplier.class);

    @Inject
    private RSAUtils rsaUtils;

    @Inject
    private ObjectMapper objectMapper;

    @Override
    public Authentication supply(@Nonnull HttpServletRequest request) {
        LoginInput loginInput = null;
        try {
            loginInput = objectMapper.readValue(request.getInputStream(), LoginInput.class);

        } catch (IOException e) {
            throw new BadCredentialsException("invalid.credentials");
        }
        String password = rsaUtils.decrypt(loginInput.getPassword());
        return new PasswordAuthentication(loginInput.getUsername(), password);
    }

    @Override
    public boolean supports(@Nonnull HttpServletRequest request) {
        return HttpMethod.POST.toString().equalsIgnoreCase(request.getMethod()) &&
                request.getContentType() != null &&
                request.getContentType().contains(MediaType.APPLICATION_JSON_VALUE) &&
                LoginController.REQUEST_MAPPING.equals(request.getRequestURI());
    }
}
