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
package com.folib.authentication.registry.support;

import com.folib.resource.ConfigurationResourceResolver;
import com.folib.util.ClassLoaderFactory;

import javax.inject.Inject;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ExternalAuthenticatorsHelper
{

    private static final Pattern AUTHENTICAION_PROVIDER_PATTERN = Pattern.compile(
            "folib-.*-authentication-provider.*jar");

    private static final Logger logger = LoggerFactory.getLogger(ExternalAuthenticatorsHelper.class);

    @Inject
    private ConfigurationResourceResolver configurationResourceResolver;


    private ExternalAuthenticatorsHelper()
    {

    }

    public ClassLoader getExternalAuthenticatorsClassLoader(ClassLoader parent)
    {
        Path authenticatorsDirectory;
        try
        {
            authenticatorsDirectory = Paths.get(
                    configurationResourceResolver.getConfigurationResource("folib.authentication.lib",
                                                                           "webapp/WEB-INF/lib").getURI());
        }
        catch (FileNotFoundException e)
        {
            logger.info(e.getMessage());
            return parent;
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
        if (!Files.exists(authenticatorsDirectory))
        {
            logger.info("{} does not exist.", authenticatorsDirectory);
            return parent;
        }
        if (!Files.isDirectory(authenticatorsDirectory))
        {
            logger.error("{} is not a directory.", authenticatorsDirectory);
            return parent;
        }

        final Set<Path> authenticatorsJarPaths = getAuthenticatorJarPaths(authenticatorsDirectory);

        if (CollectionUtils.isEmpty(authenticatorsJarPaths))
        {
            logger.info("{} does not contain any authenticator jar files.", authenticatorsDirectory);
            return parent;
        }

        return ClassLoaderFactory.urlClassLoaderFromPaths(parent, authenticatorsJarPaths);
    }

    private static Set<Path> getAuthenticatorJarPaths(final Path authenticatorsDirectory)
    {
        final Set<Path> authenticatorsJarPaths = new HashSet<>();
        try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(authenticatorsDirectory))
        {
            for (Path path : directoryStream)
            {
                if (isAuthenticatorJarPath(path))
                {
                    authenticatorsJarPaths.add(path);
                }
            }
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
        return authenticatorsJarPaths;
    }

    private static boolean isAuthenticatorJarPath(Path path)
    {
        return path != null && !Files.isDirectory(path) &&
               AUTHENTICAION_PROVIDER_PATTERN.matcher(path.getFileName().toString()).matches();
    }

}
