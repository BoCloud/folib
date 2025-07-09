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
package com.folib.services.impl;

import com.folib.components.NpmComponent;
import com.folib.enums.NpmIndexTypeEnum;
import com.folib.enums.NpmRepositoryTypeEnum;
import com.folib.indexer.NpmPackageMetadataIndexer;
import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.PackageVersion;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.NpmProvider;
import com.folib.storage.repository.Repository;
import com.folib.utils.NpmUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class NpmHostedProvider implements NpmProvider {

    @Inject
    private NpmProviderRegistry npmProviderRegistry;

    @Inject
    private NpmPackageMetadataIndexer npmPackageMetadataIndexer;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private NpmComponent npmComponent;

    @PostConstruct
    @Override
    public void register() {
        npmProviderRegistry.addProvider(NpmRepositoryTypeEnum.NPM_HOSTED.getType(), this);
        log.info("Registered npm provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), NpmRepositoryTypeEnum.NPM_HOSTED.getType());
    }

    @Override
    public PackageVersion packageVersion(Repository repository, String packageName, String version, String targetUrl) {
        PackageFeed packageFeed = packageFeed(repository, packageName, targetUrl);
        if (Objects.nonNull(packageFeed)) {
            log.debug("Attempting to find the version {} in package metadata {}", version, packageName);
            PackageVersion packageVersion = packageFeed.getVersions().getAdditionalProperties().get(version);
            if (Objects.nonNull(packageVersion)) {
                log.debug("Attempting to transform metadata content and minimize data, version {} in the package {}", version, packageName);
                return packageVersion;
            }
        }
        return null;
    }

    @Override
    public PackageVersion getLocalPackageVersion(Repository repository, String packageName, String version, String targetUrl) {
        return packageVersion(repository, packageName, version, targetUrl);
    }

    @Override
    public PackageFeed packageFeed(Repository repository, String packageName, String targetUrl) {
        String packageFeedFilePath = NpmUtils.getPackageMetadataPath(packageName);
        RepositoryPath packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageFeedFilePath);
        try {
            if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath) || RepositoryFiles.hasRefreshContent(packageMetadataRepositoryPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, packageName);
                if (!Files.exists(repositoryPath)) {
                    return null;
                }
                npmPackageMetadataIndexer.indexAsSystem(repositoryPath, NpmIndexTypeEnum.REINDEX);
                packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageFeedFilePath);
                if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath)) {
                    return null;
                }
            }
            return npmComponent.readPackageFeed(packageMetadataRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public PackageFeed getLocalPackageFeed(Repository repository, String packageName, String targetUrl) {
        return packageFeed(repository, packageName, targetUrl);
    }

    @Override
    public String binary(Repository repository, String packageName, String targetUrl) {
        return null;
    }

    @Override
    public String getLocalBinary(Repository repository, String packageName, String targetUrl) {
        return null;
    }

}
