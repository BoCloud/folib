/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
