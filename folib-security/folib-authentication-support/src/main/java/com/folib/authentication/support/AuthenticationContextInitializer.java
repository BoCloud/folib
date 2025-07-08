package com.folib.authentication.support;

import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.core.env.StandardEnvironment;

public class AuthenticationContextInitializer implements ApplicationContextInitializer<GenericApplicationContext>
{

    public static final String FOLIB_AUTHENTICATION_PROVIDERS = "folib-authentication-providers";

    private final PropertySource<?> authenticationPropertySource;

    public AuthenticationContextInitializer(PropertySource<?> propertySource)
    {
        this.authenticationPropertySource = propertySource;
    }

    @Override
    public void initialize(GenericApplicationContext authenticationContext)
    {
        ConfigurableEnvironment env = new StandardEnvironment();
        authenticationContext.setEnvironment(env);

        MutablePropertySources propertySources = env.getPropertySources();
        propertySources.addFirst(authenticationPropertySource);

        PropertySourcesPlaceholderConfigurer propertyHolder = new PropertySourcesPlaceholderConfigurer();
        propertyHolder.setEnvironment(env);
        authenticationContext.addBeanFactoryPostProcessor(propertyHolder);

        AuthenticationContextConfigurer authenticationContextConfigurer = new AuthenticationContextConfigurer();
        authenticationContextConfigurer.setEnvironment(env);
        authenticationContext.addBeanFactoryPostProcessor(authenticationContextConfigurer);
    }

}
