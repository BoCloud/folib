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
package com.folib.booters;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContextInitializer;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

/**
 * @author veadan
 */
public class ResourcesBooter implements ApplicationContextInitializer<ConfigurableApplicationContext>
{

    private static final Logger logger = LoggerFactory.getLogger(ResourcesBooter.class);

    private PathMatchingResourcePatternResolver resolver = new PathMatchingResourcePatternResolver();

    @Override
    public void initialize(ConfigurableApplicationContext applicationContext)
    {
        ConfigurableEnvironment environment = applicationContext.getEnvironment();
        try
        {
            copyConfigurationFilesFromClasspath(environment.getProperty("folib.home"), "etc/conf");
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
    }

    public Resource[] getConfigurationResourcesFromClasspath(String resourcesBasedir)
            throws IOException
    {
        return resolver.getResources("classpath*:" + resourcesBasedir + "/**/*");
    }

    public Resource[] getResourcesExistingOnClasspathOnly(Resource[] resources)
            throws IOException
    {
        List<Resource> diff = new ArrayList<>();

        for (Resource resource : resources)
        {
            logger.info(resource.getURL().toString());
            diff.add(resource);
        }

        return diff.toArray(new Resource[diff.size()]);
    }

    public void copyConfigurationFilesFromClasspath(String folibHome, String resourcesBasedir)
            throws IOException
    {
        final Resource[] resources = getResourcesExistingOnClasspathOnly(
                getConfigurationResourcesFromClasspath(resourcesBasedir));

        final Path configDir = Paths.get(folibHome, resourcesBasedir);
        if (!Files.exists(configDir))
        {
            Files.createDirectories(configDir);
        }

        for (Resource resource : resources)
        {
            Path destFile = configDir.resolve(resource.getFilename());

            if (Files.exists(destFile))
            {
                logger.info("Resource already exists, skip [{}].", destFile);
                continue;
            }

            Files.copy(resource.getInputStream(), destFile);
        }
    }

    public PathMatchingResourcePatternResolver getResolver()
    {
        return resolver;
    }

    public void setResolver(PathMatchingResourcePatternResolver resolver)
    {
        this.resolver = resolver;
    }

}
