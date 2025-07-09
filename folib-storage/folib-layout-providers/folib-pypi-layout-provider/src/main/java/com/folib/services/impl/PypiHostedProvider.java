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

import com.folib.enums.PypiIndexTypeEnum;
import com.folib.enums.PypiRepositoryTypeEnum;
import com.folib.indexer.PypiPackageMetadataIndexer;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.PypiProvider;
import com.folib.storage.repository.Repository;
import com.folib.util.PypiUtils;
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
public class PypiHostedProvider implements PypiProvider {

    @Inject
    private PypiProviderRegistry pypiProviderRegistry;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private PypiPackageMetadataIndexer pypiPackageMetadataIndexer;

    @PostConstruct
    @Override
    public void register() {
        pypiProviderRegistry.addProvider(PypiRepositoryTypeEnum.PYPI_HOSTED.getType(), this);
        log.info("Registered pypi provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), PypiRepositoryTypeEnum.PYPI_HOSTED.getType());
    }

    @Override
    public String packages(Repository repository, String packageName, String targetUrl) {
        String packageMetadataFilePath = PypiUtils.getPackageIndexPathLocalRepo(packageName);
        RepositoryPath packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
        try {
            if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath) || RepositoryFiles.hasRefreshContent(packageMetadataRepositoryPath)) {
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, packageName);
//                if (!Files.exists(repositoryPath)) {
//                    return null;
//                }
                pypiPackageMetadataIndexer.indexAsSystem(repositoryPath, PypiIndexTypeEnum.REINDEX);
                packageMetadataRepositoryPath = repositoryPathResolver.resolve(repository, packageMetadataFilePath);
                if (Objects.isNull(packageMetadataRepositoryPath) || !Files.exists(packageMetadataRepositoryPath)) {
                    return null;
                }
            }
            return Files.readString(packageMetadataRepositoryPath);
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public String getLocalPackages(Repository repository, String packageName, String targetUrl) {
        return packages(repository, packageName, targetUrl);
    }

}
