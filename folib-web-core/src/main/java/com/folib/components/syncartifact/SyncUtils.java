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
package com.folib.components.syncartifact;

import com.folib.components.DistributedCounterComponent;
import com.folib.components.artifact.ArtifactComponent;
import com.folib.components.common.CommonComponent;
import com.folib.components.files.FilesCommonComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactResolutionService;
import com.folib.services.ArtifactWebService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.Objects;

/**
 * @author veadan
 * @since 2025-01-20 16:54
 */

@Slf4j
@Component
public class SyncUtils {

    final String ARTIFACT_COUNT = "migrate:artifact:count:";
    final String INDEX_COUNT = "migrate:index:count:";
    final String DIRECTORY_COUNT="migrate:directory:count:";


    @Resource
    private CommonComponent commonComponent;


    @Resource
    public ArtifactComponent artifactComponent;

    @Value("${folib.temp}")
    public String tempPath;

    @Resource
    private DistributedCounterComponent distributedCounterComponent;

    @Resource
    private ConfigurationManager configurationManager;

    @Resource
    private FilesCommonComponent filesCommonComponent;

    @Resource
    public ArtifactResolutionService artifactResolutionService;

    @Resource
    public RepositoryPathResolver repositoryPathResolver;

    @Resource
    public ArtifactWebService artifactWebService;

    public void resetIndex(String storeAndRepo){
        distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).set(0L);
    }

    public void setIndex(String storeAndRepo,int count){
        distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).set(count);
    }

    public void resetArtifact(String storeAndRepo){
        distributedCounterComponent.getAtomicLong(ARTIFACT_COUNT + storeAndRepo).set(0L);
    }

    public void resetDirectoryCount(String storeAndRepo){
        distributedCounterComponent.getAtomicLong(DIRECTORY_COUNT + storeAndRepo).set(0L);
    }

    public void indexIncrease(String storeAndRepo) {
        distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).getAndAdd(1);
    }

    public int getIndexCount(String storeAndRepo) {
        return (int) distributedCounterComponent.getAtomicLong(INDEX_COUNT + storeAndRepo).get();
    }

    public void artifactIncrease(String storeAndRepo) {
        distributedCounterComponent.getAtomicLong(ARTIFACT_COUNT + storeAndRepo).getAndAdd(1);
    }

    public int getArtifactCount(String storeAndRepo) {
        return (int) distributedCounterComponent.getAtomicLong(ARTIFACT_COUNT + storeAndRepo).get();
    }

    public void directoryIncrease(String storeAndRepo) {
        distributedCounterComponent.getAtomicLong(DIRECTORY_COUNT + storeAndRepo).getAndAdd(1);
    }

    public int getDirectoryCount(String storeAndRepo) {
        return (int) distributedCounterComponent.getAtomicLong(DIRECTORY_COUNT + storeAndRepo).get();
    }

    public Repository validRepo(SyncArtifactForm syncArtifactForm){
        Repository repository = configurationManager.getRepository(syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
        if (Objects.isNull(repository)) {
            log.error("存储空间 【{}】 所属仓库 【{}】 仓库不存在",syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            return null;
        }
        if (!RepositoryTypeEnum.PROXY.getType().equalsIgnoreCase(repository.getType())) {
            log.error("存储空间【{}】 所属仓库 【{}】 不是代理库", syncArtifactForm.getStorageId(), syncArtifactForm.getRepositoryId());
            return null;
        }
        return repository;
    }


    public String getBaseUri(){
        return configurationManager.getBaseUri().toString();
    }

    public void storeContent(String absUrl, String path){
        filesCommonComponent.storeContent(absUrl,path);
    }

    public int getDefaultThreadNums(){
        return commonComponent.getAvailableCores() * 2;
    }

    ThreadPoolTaskExecutor createThreadPool(String name,int corePoolSize, int maxPoolSize){
        return commonComponent.buildThreadPoolTaskExecutor(name,corePoolSize,maxPoolSize);
    }

    public String getTempPath(){
        return this.tempPath;
    }


    RepositoryPath resolve(String storageId,String repositoryId, String artifactPath){
        return repositoryPathResolver.resolve(storageId,repositoryId,artifactPath);
    }

    RepositoryPath resolvePath(String storageId,String repositoryId, String artifactPath) throws IOException {
        return artifactResolutionService.resolvePath(storageId,repositoryId,artifactPath);
    }

    public void saveArtifactMetaByString(String storageId,String repositoryId,String path,String metaData){
        artifactWebService.saveArtifactMetaByString(storageId,repositoryId,path,metaData);
    }
}
