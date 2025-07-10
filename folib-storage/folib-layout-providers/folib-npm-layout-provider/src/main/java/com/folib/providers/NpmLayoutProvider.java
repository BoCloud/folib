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
package com.folib.providers;

import java.io.IOException;
import java.nio.file.Files;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import cn.hutool.crypto.digest.SM3;
import com.folib.providers.io.RepositoryFileAttributeType;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.NpmRepositoryFeatures;
import com.folib.repository.NpmRepositoryStrategy;
import com.folib.repository.RepositoryStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;

import com.folib.artifact.coordinates.NpmCoordinates;

import org.apache.commons.lang3.BooleanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author @author veadan
 */
@Component
public class NpmLayoutProvider
        extends AbstractLayoutProvider<NpmCoordinates>
{

    private static final Logger logger = LoggerFactory.getLogger(NpmLayoutProvider.class);

    public static final String ALIAS = NpmCoordinates.LAYOUT_NAME;

    public static final String NPM_USER_PATH = "-/user/org.couchdb.user:";

    public static final String  PACKAGE_JSON = "package.json";

    public static final String  OH_PACKAGE_JSON = "oh-package.json5";

    public static final String  DEFAULT_PACKAGE_JSON_PATH = "package/package.json";
    public static final String  OHPM_PACKAGE_JSON_PATH = "package/oh-package.json5";
    public static final String DEFAULT_SUFFIX = "tgz";

    public static final Pattern NPM_URL_USERNAME_PATTERN = Pattern.compile(
            "(?:" + NpmLayoutProvider.NPM_USER_PATH + ")(.*)");

    @Lazy
    @Inject
    private NpmRepositoryStrategy npmRepositoryManagementStrategy;
    @Lazy
    @Inject
    private NpmRepositoryFeatures npmRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.", getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public NpmCoordinates getArtifactCoordinates(RepositoryPath path)
            throws IOException
    {
        return NpmCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    public boolean isArtifactMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith(PACKAGE_JSON) || path.getFileName().toString().endsWith(OH_PACKAGE_JSON);
    }

    public boolean isNpmMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith("package-lock.json") ||
               path.getFileName().toString().endsWith("npm-shrinkwrap.json");
    }

    public boolean isNpmPackageJson(RepositoryPath path) {
        return path.getFileName().toString().endsWith(".json");
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryPath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException
    {
        Map<RepositoryFileAttributeType, Object> result = super.getRepositoryFileAttributes(repositoryPath,
                                                                                            attributeTypes);

        for (RepositoryFileAttributeType attributeType : attributeTypes)
        {
            Object value = result.get(attributeType);
            switch (attributeType)
            {
                case ARTIFACT:
                    value = (Boolean) value && !isNpmMetadata(repositoryPath);

                    if (value != null)
                    {
                        result.put(attributeType, value);
                    }

                    break;
                case METADATA:
                    value = (Boolean) value || isNpmMetadata(repositoryPath);

                    if (value != null)
                    {
                        result.put(attributeType, value);
                    }

                    break;
                case REFRESH_CONTENT:
                    final Instant halfAnHourAgo = Instant.now().minus(refreshContentInterval(repositoryPath), ChronoUnit.MINUTES);
                    value = BooleanUtils.isTrue((Boolean) value) || (Files.exists(repositoryPath) && isNpmPackageJson(repositoryPath)
                            &&
                            !RepositoryFiles.wasModifiedAfter(repositoryPath,
                                    halfAnHourAgo));
                    result.put(attributeType, value);
                    break;
                default:

                    break;
            }
        }

        return result;
    }

    @Override
    public RepositoryStrategy getRepositoryManagementStrategy()
    {
        return npmRepositoryManagementStrategy;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return npmRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet()
    {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1, MessageDigestAlgorithms.SHA_256, MessageDigestAlgorithms.SHA_512, SM3.ALGORITHM_NAME)
                     .collect(Collectors.toSet());
    }

}
