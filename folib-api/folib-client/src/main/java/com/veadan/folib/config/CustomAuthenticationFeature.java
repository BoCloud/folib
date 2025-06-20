package com.veadan.folib.config;

import com.veadan.folib.filter.Utf8BasicAuthFilter;
import org.springframework.util.StringUtils;

import javax.ws.rs.core.Feature;
import javax.ws.rs.core.FeatureContext;

/**
 * @author leipenghui
 * @date 2025/4/11
 **/
public class CustomAuthenticationFeature implements Feature {

    private final String username;
    private final String password;

    public CustomAuthenticationFeature(String username, String password) {
        this.username = username;
        this.password = password;
    }

    public static CustomAuthenticationFeature create(String username, String password) {
        return new CustomAuthenticationFeature(username, password);
    }

    @Override
    public boolean configure(FeatureContext context) {
        if (!StringUtils.hasText(username) || !StringUtils.hasText(password)) {
            return false;
        }
        context.register(new Utf8BasicAuthFilter(username, password));
        return true;
    }
}