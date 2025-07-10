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
package com.folib.resource;

import com.folib.booters.PropertiesBooter;

import jakarta.inject.Inject;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.Environment;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ConfigurationResourceResolver
{

    private static final Logger logger = LoggerFactory.getLogger(ConfigurationResourceResolver.class);

    @Inject
    private PropertiesBooter propertiesBooter;
    
    @Inject
    private Environment environment;


    public Resource getConfigurationResource(String propertyKey,
                                             String propertyDefaultValue)
    {
        final String configurationPath = propertyDefaultValue != null && !propertyDefaultValue.startsWith("classpath:") ?
                                         propertiesBooter.getHomeDirectory() + "/" + propertyDefaultValue :
                                         propertyDefaultValue;

        return getConfigurationResource(configurationPath, propertyKey, propertyDefaultValue);
    }

    /**
     * @param configurationPath
     *            The configuration file's path. If null, either propertyKey,
     *            or propertyKeyDefaultValue must be specified.
     * @param propertyKey
     *            The system property key to use when trying to load the
     *            configuration.
     * @param propertyDefaultValue
     *            The default property key value.
     * @return
     * @throws IOException
     */
    public Resource getConfigurationResource(String configurationPath,
                                             String propertyKey,
                                             String propertyDefaultValue)
    {
        DefaultResourceLoader resourceLoader = new DefaultResourceLoader();
        resourceLoader.addProtocolResolver((l,
                                            r) -> {

            File file = new File(l);
            if (!file.exists())
            {
                return null;
            }

            return new FileSystemResource(file);
        });

        String filename;
        if ((filename = getProperty(propertyKey)) != null)
        {
            logger.info("Using configured resource path [{}]", filename);

            return resourceLoader.getResource(filename);
        }

        logger.info("Try to fetch configuration resource path [{}]", configurationPath);

        if (configurationPath != null &&
            (!configurationPath.startsWith("classpath") && !(Files.exists(Paths.get(configurationPath)))))
        {
            logger.info("Configuration resource [{}] does not exist, will try to resolve with configured location [{}].",
                        configurationPath, propertyKey);

            configurationPath = null;
        }

        if (configurationPath != null)
        {
            logger.info("Using provided resource path [{}]", configurationPath);

            return resourceLoader.getResource(configurationPath);
        }
        else
        {
            logger.info("Using default resource path [{}]", propertyDefaultValue);

            return resourceLoader.getResource(propertyDefaultValue);
        }
    }

    private String getProperty(String propertyKey)
    {
        return environment.getProperty(propertyKey);
    }

}
