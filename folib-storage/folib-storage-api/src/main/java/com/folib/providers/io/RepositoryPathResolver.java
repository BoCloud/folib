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

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.configuration.ConfigurationManager;
import com.folib.domain.Artifact;
import com.folib.repositories.ArtifactRepository;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.util.CacheUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.util.Objects;
import java.util.Optional;

/**
 * @author veadan
 */
@Slf4j
@Component
public class RepositoryPathResolver {
    @Lazy
    @Inject
    protected ConfigurationManager configurationManager;

    @Lazy
    @Inject
    protected ArtifactRepository artifactEntityRepository;
    @Lazy
    @Inject
    protected RepositoryFileSystemRegistry fileSystemRegistry;

    public RootRepositoryPath resolve(String storageId,
                                      String repositoryId) {
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            Objects.requireNonNull(storage, String.format("Storage [%s] not found", storageId));
            repository = storage.getRepository(repositoryId);
            Objects.requireNonNull(repository, String.format("Repository [%s] [%s] not found", storageId, repositoryId));
            cacheUtil.put(key, repository);
        }
        return resolve(repository);
    }

    public RootRepositoryPath resolve(final Repository repository) {
        Objects.requireNonNull(repository, "Repository should be provided");

        LayoutFileSystemFactory fileSystemFactory = fileSystemRegistry.lookupRepositoryFileSystemFactory(repository);

        return fileSystemFactory.create(repository).getRootDirectory();
    }

    public RepositoryPath resolve(String storageId,
                                  String repositoryId,
                                  String path) {
        CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
        String key = String.format("%s:%s", storageId, repositoryId);
        Repository repository = cacheUtil.get(key);
        if (Objects.isNull(repository)) {
            Storage storage = configurationManager.getConfiguration().getStorage(storageId);
            Objects.requireNonNull(storage, String.format("Storage [%s] not found", storageId));
            repository = storage.getRepository(repositoryId);
            Objects.requireNonNull(repository, String.format("Repository [%s] [%s] not found", storageId, repositoryId));
            cacheUtil.put(key, repository);
        }
        return resolve(repository, path);
    }

    public RepositoryPath resolve(final Repository repository,
                                  final ArtifactCoordinates c) {
        return resolve(repository, c.buildPath());
    }

    public RepositoryPath resolve(final Repository repository,
                                  final RepositoryPath otherPath)
            throws IOException {
        if (otherPath.getRepository().getId().equals(repository.getId())
                && otherPath.getArtifactEntry() != null) {
            return otherPath;
        }

        return resolve(repository, RepositoryFiles.relativizePath(otherPath));
    }

    public RepositoryPath resolve(final Repository repository,
                                  final String path) {
        RootRepositoryPath repositoryPath = resolve(repository);
        if (repository.isGroupRepository()) {
            return repositoryPath.resolve(path);
        }

        return new LazyRepositoryPath(repositoryPath.resolve(path));
    }

    public Artifact findOneArtifact(String storageId, String repositoryId, String path) {
        return artifactEntityRepository.findOneArtifact(storageId, repositoryId, path);
    }

    private class LazyRepositoryPath extends RepositoryPath {

        private LazyRepositoryPath(RepositoryPath target) {
            super(target.getTarget(), target.getFileSystem());
            this.artifact = target.artifact;
            this.artifactExist = target.artifactExist;
            this.extAttribute=target.extAttribute;
        }

        @Override
        public Artifact getArtifactEntry()
                throws IOException {
            Artifact artifactLocal = super.getArtifactEntry();
            if (artifactLocal == NullArtifact.INSTANCE) {
                return null;
            } else if (artifactLocal != null) {
                return artifactLocal;
            }

            if (this.getRepository().isGroupRepository() || !RepositoryFiles.isArtifact(this)) {
                artifact = NullArtifact.INSTANCE;
            } else {
                artifact = Optional.ofNullable(artifactEntityRepository.findOneArtifact(getRepository().getStorage().getId(),
                        getRepository().getId(),
                        RepositoryFiles.relativizePath(this)))
                        .orElse(NullArtifact.INSTANCE);
            }

            return getArtifactEntry();
            // TODO: we should check this restriction 
//            if (Files.exists(this) && !Files.isDirectory(this) && RepositoryFiles.isArtifact(this) && result == null)
//            {
//                throw new IOException(String.format("Corresponding [%s] record not found for path [%s]",
//                                                    ArtifactEntry.class.getSimpleName(), this));
//            }

        }

        @Override
        public Boolean getArtifactExist() throws IOException {
            Boolean artifactExistLocal = super.getArtifactExist();
            if (Objects.nonNull(artifactExistLocal)) {
                return artifactExistLocal;
            }

            if (this.getRepository().isGroupRepository() || !RepositoryFiles.isArtifact(this)) {
                artifactExist = false;
            } else {
                artifactExist = Optional.ofNullable(artifactEntityRepository.artifactExists(getRepository().getStorage().getId(),
                        getRepository().getId(),
                        RepositoryFiles.relativizePath(this)))
                        .orElse(false);
            }

            return getArtifactExist();
        }

        @Override
        public RepositoryPath normalize() {
            RepositoryPath target = super.normalize();
            return new LazyRepositoryPath(target);
        }

    }

    private static class NullArtifact {

        private static final Artifact INSTANCE = (Artifact) Proxy.newProxyInstance(Artifact.class.getClassLoader(),
                new Class[]{Artifact.class},
                (proxy,
                 method,
                 args) -> {
                    throw new UnsupportedOperationException();
                });

    }

}
