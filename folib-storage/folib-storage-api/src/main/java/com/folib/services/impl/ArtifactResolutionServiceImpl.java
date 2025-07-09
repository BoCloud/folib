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

import com.folib.artifact.ArtifactNotFoundException;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RepositoryStreamSupport;
import com.folib.providers.repository.RepositoryProvider;
import com.folib.providers.repository.RepositoryProviderRegistry;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactResolutionService;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.resource.ArtifactOperationsValidator;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

/**
 * @author veadan
 */
@Component
public class ArtifactResolutionServiceImpl
        implements ArtifactResolutionService {

    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private ArtifactOperationsValidator artifactOperationsValidator;
    @Lazy
    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Lazy
    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Override
    public RepositoryStreamSupport.RepositoryInputStream getInputStream(RepositoryPath path)
            throws IOException {
        Repository repository = path.getFileSystem().getRepository();
//        artifactOperationsValidator.validate(path);

        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        return (RepositoryStreamSupport.RepositoryInputStream) repositoryProvider.getInputStream(path);
    }

    @Override
    public RepositoryStreamSupport.RepositoryStoreIndexInputStream getStoreIndexInputStream(RepositoryPath path) throws IOException {
        Repository repository = path.getFileSystem().getRepository();
        artifactOperationsValidator.validate(path);

        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        return (RepositoryStreamSupport.RepositoryStoreIndexInputStream) repositoryProvider.getStoreIndexInputStream(path);
    }

    @Override
    public RepositoryStreamSupport.RepositoryOutputStream getOutputStream(RepositoryPath repositoryPath)
            throws IOException,
            NoSuchAlgorithmException {
        artifactOperationsValidator.validate(repositoryPath);

        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());

        RepositoryStreamSupport.RepositoryOutputStream os = (RepositoryStreamSupport.RepositoryOutputStream) repositoryProvider.getOutputStream(repositoryPath);
        if (os == null) {
            throw new ArtifactStorageException("Artifact " + repositoryPath + " cannot be stored.");
        }
        return os;
    }

    public Storage getStorage(String storageId) {
        return configurationManager.getConfiguration().getStorage(storageId);
    }

    @Override
    public RepositoryPath resolvePath(String storageId,
                                      String repositoryId,
                                      String artifactPath)
            throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) repositoryProvider.fetchPath(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            return null;
        }
    }

    @Override
    public RepositoryPath resolvePath(String storageId, String repositoryId, String targetUrl, String artifactPath) throws IOException {
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, artifactPath);
        repositoryPath.setTargetUrl(targetUrl);
        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) repositoryProvider.fetchPath(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            return null;
        }
    }

    @Override
    public RepositoryPath resolvePath(RepositoryPath repositoryPath) throws IOException {
        Repository repository = repositoryPath.getRepository();
        RepositoryProvider repositoryProvider = repositoryProviderRegistry.getProvider(repository.getType());
        try {
            return (RepositoryPath) repositoryProvider.fetchPath(repositoryPath);
        } catch (ArtifactNotFoundException e) {
            return null;
        }
    }

}
