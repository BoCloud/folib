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
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import com.folib.artifact.coordinates.NugetCoordinates;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.AbstractLayoutProvider;
import com.folib.repository.NugetRepositoryFeatures;
import com.folib.repository.NugetRepositoryStrategy;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * Layout provider for Nuget package repository.<br>
 * It provides hierarchical directory layout like follows: <br>
 * &lt;packageID&gt;<br>
 * └─&lt;version&gt;<br>
 * &emsp;├─&lt;packageID&gt;.&lt;version&gt;.nupkg<br>
 * &emsp;├─&lt;packageID&gt;.&lt;version&gt;.nupkg.sha512<br>
 * &emsp;└─&lt;packageID&gt;.nuspec<br>
 * 
 * 
 * @author @author veadan
 *
 */
@Component
public class NugetLayoutProvider
        extends AbstractLayoutProvider<NugetCoordinates>
{
    private static final Logger logger = LoggerFactory.getLogger(NugetLayoutProvider.class);

    public static final String ALIAS = NugetCoordinates.LAYOUT_NAME;

    public static final String USER_AGENT_PREFIX = ALIAS;

    @Lazy
    @Inject
    private NugetRepositoryStrategy nugetRepositoryManagementStrategy;
    @Lazy
    @Inject
    private NugetRepositoryFeatures nugetRepositoryFeatures;


    @PostConstruct
    public void register()
    {
        logger.info("Registered layout provider '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public NugetCoordinates getArtifactCoordinates(RepositoryPath path) throws IOException
    {
        return NugetCoordinates.parse(RepositoryFiles.relativizePath(path));
    }

    @Override
    protected boolean isArtifactMetadata(RepositoryPath path)
    {
        return path.getFileName().toString().endsWith(".nuspec");
    }

    protected String toBase64(byte[] digest)
    {
        byte[] encoded = Base64.getEncoder()
                               .encode(digest);
        return new String(encoded, StandardCharsets.UTF_8);
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return nugetRepositoryFeatures.getDefaultArtifactCoordinateValidators();
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public Set<String> getDigestAlgorithmSet()
    {
        return Stream.of(MessageDigestAlgorithms.SHA_512)
                     .collect(Collectors.toSet());
    }

    @Override
    public NugetRepositoryStrategy getRepositoryManagementStrategy()
    {
        return nugetRepositoryManagementStrategy;
    }

}
