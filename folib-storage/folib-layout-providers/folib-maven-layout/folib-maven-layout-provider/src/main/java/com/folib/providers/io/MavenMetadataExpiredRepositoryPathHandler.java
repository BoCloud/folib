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
package com.folib.providers.io;

import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.folib.storage.metadata.maven.ChecksumMetadataExpirationStrategy;
import com.folib.storage.metadata.maven.MetadataExpirationStrategy;
import com.folib.storage.metadata.maven.MetadataExpirationStrategyType;
import com.folib.storage.metadata.maven.RefreshMetadataExpirationStrategy;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import com.folib.configuration.MavenRepositoryConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.util.Optional;

import static com.folib.storage.metadata.maven.MetadataExpirationStrategy.Decision.USABLE;

/**
 * @author veadan
 */
@Component
public class MavenMetadataExpiredRepositoryPathHandler
        implements MavenExpiredRepositoryPathHandler {

    private static final Logger logger = LoggerFactory.getLogger(MavenMetadataExpiredRepositoryPathHandler.class);

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    private ChecksumMetadataExpirationStrategy checksumMetadataExpirationStrategy;

    @Inject
    private RefreshMetadataExpirationStrategy refreshMetadataStrategy;

    @Override
    public boolean supports(final RepositoryPath repositoryPath)
            throws IOException {
        if (repositoryPath == null) {
            return false;
        }

        if (!Maven2LayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout()) || !RepositoryFiles.isMetadata(repositoryPath)) {
            return false;
        }

        Repository repository = repositoryPath.getRepository();
        return ((RepositoryData) repository).getRemoteRepository() != null;
    }

    @Override
    public void handleExpiration(final RepositoryPath repositoryPath)
            throws IOException {
        MetadataExpirationStrategy metadataExpirationStrategy = getMetadataStrategy(repositoryPath);
        MetadataExpirationStrategy.Decision refetchMetadata = metadataExpirationStrategy.decide(repositoryPath);

        if (refetchMetadata == USABLE) {
            return;
        }
        proxyRepositoryArtifactResolver.fetchRemoteResource(repositoryPath);
    }

    private MetadataExpirationStrategy getMetadataStrategy(final RepositoryPath repositoryPath) {
        MavenRepositoryConfiguration repositoryConfiguration =
                (MavenRepositoryConfiguration) repositoryPath.getRepository().getRepositoryConfiguration();

        String strategy = Optional.ofNullable(repositoryConfiguration)
                .map(MavenRepositoryConfiguration::getMetadataExpirationStrategy)
                .orElse(null);

        if (MetadataExpirationStrategyType.REFRESH == MetadataExpirationStrategyType.ofStrategy(strategy)) {
            return refreshMetadataStrategy;
        }

        return checksumMetadataExpirationStrategy;
    }

}
