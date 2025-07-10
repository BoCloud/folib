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
package com.folib.storage.validation.resource;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.ProviderImplementationException;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.repositories.ArtifactRepository;
import com.folib.storage.ArtifactResolutionException;
import com.folib.storage.ArtifactStorageException;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import jakarta.inject.Inject;

import java.io.IOException;
import java.time.Duration;
import java.time.Instant;

/**
 * @author veadan
 */
@Slf4j
@Component("artifactOperationsValidator")
public class ArtifactOperationsValidator {

    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Lazy
    @Inject
    private ArtifactRepository artifactRepository;
    @Lazy
    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    private static final long MINUTES_TO_MILLIS = 1L;

    public ArtifactOperationsValidator() {
    }

    public void validate(RepositoryPath repositoryPath)
            throws ArtifactResolutionException {
        checkArtifactPath(repositoryPath);

        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();

        checkStorageExists(storage.getId());
        checkRepositoryExists(storage.getId(), repository.getId());
    }

    public void checkStorageExists(String storageId)
            throws ArtifactResolutionException {
        if (storageId == null) {
            throw new ArtifactResolutionException("No storage specified.");
        }

        if (getConfiguration().getStorage(storageId) == null) {
            throw new ArtifactResolutionException("Storage " + storageId + " does not exist.");
        }
    }

    public void checkRepositoryExists(String storageId,
                                      String repositoryId)
            throws ArtifactResolutionException {
        if (repositoryId == null) {
            throw new ArtifactResolutionException("No repository specified.");
        }

        if (getConfiguration().getStorage(storageId)
                .getRepository(repositoryId) == null) {
            throw new ArtifactResolutionException("Repository " + repositoryId + " does not exist.");
        }
    }

    public void checkArtifactPath(RepositoryPath repositoryPath)
            throws ArtifactResolutionException {
        if (repositoryPath == null) {
            throw new ArtifactResolutionException("No artifact path specified.");
        }
    }

    public void checkAllowsDeployment(Repository repository)
            throws ArtifactStorageException {
        if (!repository.isAllowsDeployment() ||
                RepositoryTypeEnum.GROUP.getType().equals(repository.getType()) ||
                RepositoryTypeEnum.PROXY.getType().equals(repository.getType())) {
            // It should not be possible to write artifacts to:
            // - a repository that doesn't allow the deployment of artifacts
            // - a proxy repository
            // - a group repository
            //
            // NOTE:
            // - A proxy repository should only serve artifacts that already exist in the cache, or the remote host.
            // - Both the ProxyRepositoryProvider and GroupRepositoryProvider need to have an implementation of the
            //   getOutputStream(...) method, which is why this check is performed here instead.

            throw new ArtifactStorageException("Deployment of artifacts to " + repository.getType() +
                    " repositories is not allowed!");
        }
    }

    public void checkAllowsRedeployment(Repository repository,
                                        ArtifactCoordinates coordinates)
            throws IOException,
            ProviderImplementationException {
        LayoutProvider layoutProvider = LayoutProviderRegistry.getLayoutProvider(repository, layoutProviderRegistry);

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        if (RepositoryFiles.artifactExists(repositoryPath) && !repository.isAllowsRedeployment()) {
            throw new ArtifactStorageException("Re-deployment of artifacts to " +
                    repository.getStorage().getId() + ":" + repository.getId() +
                    " repository is not allowed!");
        }
    }

    public void checkAllowsDeletion(Repository repository)
            throws ArtifactStorageException {
        if (!repository.isAllowsDeletion()) {
            throw new ArtifactStorageException("Deleting artifacts from " + repository.getType() +
                    " repository is not allowed!");
        }
    }

    public void checkArtifactSize(String storageId,
                                  String repositoryId,
                                  MultipartFile uploadedFile)
            throws ArtifactResolutionException {
        if (uploadedFile.isEmpty() || uploadedFile.getSize() == 0) {
            throw new ArtifactResolutionException("Uploaded file is empty.");
        }

        Repository repository = getConfiguration().getStorage(storageId).getRepository(repositoryId);
        long artifactMaxSize = repository.getArtifactMaxSize();

        if (artifactMaxSize > 0 && uploadedFile.getSize() > artifactMaxSize) {
            throw new ArtifactResolutionException("The size of the artifact exceeds the maximum size accepted by " +
                    "this repository (" + uploadedFile.getSize() + "/" +
                    artifactMaxSize + ").");
        }
    }

    /**
     * 根据给定的键从分布式缓存中获取内容刷新间隔设置
     * 如果没有找到对应的值或者值为空，则返回默认的内容刷新间隔
     *
     * @param key 用于从分布式缓存中检索刷新间隔设置的键
     * @return 刷新间隔设置，如果未找到或值为空，则返回默认值
     */
    public int refreshContentInterval(final String key) {
        // 从分布式缓存中获取与给定键相关的刷新间隔设置
        String refreshContentInterval = distributedCacheComponent.get(key);
        // 如果获取的刷新间隔为空或仅为空白字符，则返回预设的默认刷新间隔
        if (StringUtils.isBlank(refreshContentInterval)) {
            return 1;
        }
        // 将获取的刷新间隔字符串解析为整数并返回
        return Integer.parseInt(refreshContentInterval);
    }

    /**
     * 设置最后一次刷新时间
     * @param key key
     * @param lastTime 最后一次刷新时间
     */
    public void setLastTime(String key, long lastTime) {
        distributedCacheComponent.put(key, Long.toString(lastTime));
    }

    /**
     * 删除最后一次刷新时间
     * @param key key
     */
    public void removeLastTime(String key) {
        distributedCacheComponent.delete(key);
    }

    /**
     * 获取最后一次刷新时间
     * @param key key
     * @return 最后一次刷新时间
     */
    public Long getLastTime(String key) {
        String lastTime = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(lastTime)) {
            return null;
        }
        return Long.parseLong(lastTime);
    }

    /**
     * 判断是否需要刷新存储统计数据
     * 该方法通过比较当前时间与上次刷新时间，来决定是否需要进行刷新
     * 如果上次刷新时间为空，则自动设置当前时间为新的刷新时间，并返回true表示需要刷新
     * 如果当前时间与上次刷新时间的时间差大于等于预设的刷新间隔时间，则进行刷新并更新刷新时间
     * 否则，返回false表示不需要刷新
     * @param key key
     * @param intervalKey intervalKey
     * @return true，如果需要刷新缓存统计数据；否则返回false
     */
    public boolean isRefresh(String key, String intervalKey) {
        // 获取当前时间的瞬时值
        Instant now = Instant.now();
        // 获取上次刷新时间的毫秒值
        Long pastTimeMilli = getLastTime(key);
        // 如果上次刷新时间为空，则设置当前时间为新的刷新时间，并返回true表示需要刷新
        if (pastTimeMilli == null) {
            setLastTime(key, now.toEpochMilli());
            return true;
        }
        // 将上次刷新时间的毫秒值转换为瞬时值
        Instant pastTime = Instant.ofEpochMilli(pastTimeMilli);
        // 计算当前时间与上次刷新时间之间的时间差
        Duration duration = Duration.between(pastTime, now);
        // 计算刷新间隔时间的毫秒值
        long requiredMillis = refreshContentInterval(intervalKey) * MINUTES_TO_MILLIS;
        // 如果时间差大于等于刷新间隔时间，则进行刷新并更新刷新时间
        if (duration.compareTo(Duration.ofMillis(requiredMillis)) >= 0) {
            setLastTime(key, now.toEpochMilli());
            return true;
        }
        // 不需要刷新，返回false
        return false;
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
