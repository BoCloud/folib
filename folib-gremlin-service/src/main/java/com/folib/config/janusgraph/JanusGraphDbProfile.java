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
package com.folib.config.janusgraph;

import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;

import com.folib.util.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;
import org.springframework.core.io.FileSystemResource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;

import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;

public class JanusGraphDbProfile
{

    private static final Logger logger = LoggerFactory.getLogger(JanusGraphDbProfile.class);

    public static final String PROPERTY_PROFILE = "folib.db.profile";

    public static final String PROFILE_MEMORY = "db_MEMORY";

    public static final String PROFILE_EMBEDDED = "db_EMBEDDED";

    public static final String PROFILE_REMOTE = "db_REMOTE";

    private final String name;
    private final YAMLMapper yamlMapper = new YAMLMapper();
    private final PathMatchingResourcePatternResolver resourceResolver = new PathMatchingResourcePatternResolver();

    public JanusGraphDbProfile(String name)
    {
        this.name = name;
    }

    public String getName()
    {
        return name;
    }

    public Map<String, Object> loadConfiguration(String folibHome)
        throws IOException
    {
        String classPathResourceName = "classpath:etc/conf/" + name + ".yaml";
        Optional<Resource> dbConfigResourceOptional = Optional.ofNullable(folibHome)
                                                              .map(p -> Paths.get(p, "etc/conf"))
                                                              .map(p -> p.resolve(name + ".yaml"))
                                                              .map(FileSystemResource::new)
                                                              .map(Resource.class::cast);
        dbConfigResourceOptional.ifPresent(path -> logger.info("Checking JanusGraph DB config from [{}].", path));
        dbConfigResourceOptional = dbConfigResourceOptional.filter(Resource::exists);
        dbConfigResourceOptional.ifPresent(path -> logger.info("Using JanusGraph DB config from [{}].", path));
        Resource dbConfigResource = dbConfigResourceOptional.orElseGet(() -> {
            logger.info("Using JanusGraph DB config from [{}].", classPathResourceName);
            return resourceResolver.getResource(classPathResourceName);
        });

        byte[] dbConfigContent = IOUtils.toByteArray(dbConfigResource.getInputStream());
        if (dbConfigContent == null || dbConfigContent.length == 0)
        {
            return Collections.emptyMap();
        }

        Map<String, Object> dbProfilePropertiesMap = yamlMapper.readValue(dbConfigContent,
                                                                          Map.class);

        return CollectionUtils.flattenMap(dbProfilePropertiesMap);
    }

    public static JanusGraphDbProfile resolveProfile(ConfigurableEnvironment environment)
    {
        try
        {
            return bootstrap(environment);
        }
        catch (IOException e)
        {
            throw new UndeclaredThrowableException(e);
        }
    }

    private static JanusGraphDbProfile bootstrap(ConfigurableEnvironment environment)
        throws IOException
    {
        String profileName = environment.getProperty(PROPERTY_PROFILE, PROFILE_MEMORY);
        logger.info("Bootstrap JanusGraph DB config with profile [{}].", profileName);

        JanusGraphDbProfile profile = new JanusGraphDbProfile(profileName);
        Map<String, Object> dbProfilePropertiesMap = profile.loadConfiguration(environment.getProperty("folib.home"));

        MapPropertySource propertySource = new MapPropertySource("folib-db-profile", dbProfilePropertiesMap);
        environment.getPropertySources().addLast(propertySource);

        return profile;
    }

}
