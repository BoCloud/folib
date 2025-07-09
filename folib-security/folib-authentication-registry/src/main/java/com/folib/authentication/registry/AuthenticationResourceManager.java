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
package com.folib.authentication.registry;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.folib.resource.ConfigurationResourceResolver;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.WritableResource;
import org.springframework.security.util.InMemoryResource;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;

@Component
public class AuthenticationResourceManager
{

    private static final Logger logger = LoggerFactory.getLogger(AuthenticationResourceManager.class);

    private static final String PROPERTY_AUTHENTICATION_PROVIDERS_LOCATION = "folib.config.file.authentication.providers";

    private static final String DEFAULT_AUTHENTICATION_PROVIDERS_LOCATION = "classpath:folib-authentication-providers.xml";

    private static final String PROPERTY_AUTHENTICATION_PROVIDERS_CONFIG_LOCATION = "folib.authentication.providers.yaml";

    private static final String DEFAULT_AUTHENTICATION_PROVIDERS_CONFIG_LOCATION = "/etc/conf/folib-authentication-providers.yaml";

    private Resource authenticationConfigurationResource;

    private Resource authenticationPropertiesResource;

    @Inject
    private ConfigurationResourceResolver configurationResourceResolver;


    @PostConstruct
    public void init()
        throws IOException
    {
        authenticationConfigurationResource = doGetResource(configurationResourceResolver.getConfigurationResource(PROPERTY_AUTHENTICATION_PROVIDERS_LOCATION,
                                                                                                                   DEFAULT_AUTHENTICATION_PROVIDERS_LOCATION));

        authenticationPropertiesResource = doGetResource(configurationResourceResolver.getConfigurationResource(PROPERTY_AUTHENTICATION_PROVIDERS_CONFIG_LOCATION,
                                                                                                                DEFAULT_AUTHENTICATION_PROVIDERS_CONFIG_LOCATION));
    }

    public Resource getAuthenticationConfigurationResource()
        throws IOException
    {
        return authenticationConfigurationResource;
    }

    public Resource getAuthenticationPropertiesResource()
        throws IOException
    {
        return authenticationPropertiesResource;
    }

    private Resource doGetResource(Resource resource)
        throws IOException
    {
        if (!resource.isFile() || resource instanceof ClassPathResource)
        {
            return new InMemoryResource(StreamUtils.copyToByteArray(resource.getInputStream()));
        }

        return resource;
    }

    public void storeAuthenticationConfigurationResource(Resource resource,
                                                         InputStream is)
        throws IOException
    {
        if (!(resource instanceof WritableResource))
        {
            logger.warn("Could not store read-only resource [{}].", resource);

            return;
        }

        WritableResource writableResource = (WritableResource) resource;
        OutputStream os = writableResource.getOutputStream();

        IOUtils.copy(is, os);
    }

}
