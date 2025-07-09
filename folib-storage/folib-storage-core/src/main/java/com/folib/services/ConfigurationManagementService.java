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
package com.folib.services;

import com.folib.client.MutableRemoteRepositoryRetryArtifactDownloadConfiguration;
import com.folib.configuration.*;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.routing.MutableRoutingRule;
import com.folib.storage.routing.MutableRoutingRules;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * @author veadan
 */
public interface ConfigurationManagementService {

    MutableConfiguration getMutableConfigurationClone();

    Configuration getConfiguration();

    void setConfiguration(MutableConfiguration configuration) throws IOException;

    void setInstanceName(String instanceName) throws IOException;

    void setBaseUrl(String baseUrl) throws IOException;

    void setPort(int port) throws IOException;
    
    void setKbps(Integer kbps) throws IOException;
    
    void setSliceMbSize(Long sliceMbSize) throws IOException;

    void setAdvancedConfiguration(MutableAdvancedConfiguration advancedConfiguration) throws IOException;

    void setProxyConfiguration(String storageId,
                               String repositoryId,
                               MutableProxyConfiguration proxyConfiguration) throws IOException;

    void createStorage(StorageDto storage) throws IOException;

    void addStorageIfNotExists(StorageDto storage) throws IOException;

    void removeStorage(String storageId) throws IOException;

    void saveRepository(String storageId,
                        RepositoryDto repository) throws IOException;

    void setRepositoryBasedir(String storageId,
                        RepositoryDto repository) throws IOException;

    void removeRepositoryFromAssociatedGroups(String storageId,
                                              String repositoryId) throws IOException;

    void addRepositoryVulnerabilityWhites(String storageId, String repositoryId,
                                          Set<String> whites) throws IOException;

    void removeRepositoryVulnerabilityWhites(String storageId, String repositoryId,
                                             Set<String> whites) throws IOException;

    void addRepositoryVulnerabilityBlacks(String storageId, String repositoryId,
                                          Set<String> blacks) throws IOException;

    void removeRepositoryVulnerabilityBlacks(String storageId, String repositoryId,
                                             Set<String> blacks) throws IOException;

    void setRepositoryVulnerabilityWhites(String storageId, String repositoryId,
                                          Set<String> whites) throws IOException;

    void setRepositoryVulnerabilityBlacks(String storageId, String repositoryId,
                                          Set<String> blacks) throws IOException;

    void removeRepository(String storageId,
                          String repositoryId) throws IOException;

    void setProxyRepositoryMaxConnections(String storageId,
                                          String repositoryId,
                                          int numberOfConnections) throws IOException;

    MutableRoutingRules getRoutingRules();

    MutableRoutingRule getRoutingRule(UUID uuid);

    boolean updateRoutingRule(UUID uuid,
                              MutableRoutingRule routingRule) throws IOException;

    boolean addRoutingRule(MutableRoutingRule routingRule) throws IOException;

    boolean removeRoutingRule(UUID uuid) throws IOException;

    void addRepositoryToGroup(String storageId,
                              String repositoryId,
                              String repositoryGroupMemberId) throws IOException;

    void setRepositoryArtifactCoordinateValidators() throws IOException;

    void putInService(String storageId,
                      String repositoryId) throws IOException;

    void putOutOfService(String storageId,
                         String repositoryId) throws IOException;

    void setArtifactMaxSize(String storageId,
                            String repositoryId,
                            long value) throws IOException;

    void set(MutableRemoteRepositoryRetryArtifactDownloadConfiguration remoteRepositoryRetryArtifactDownloadConfiguration) throws IOException;

    void addRepositoryArtifactCoordinateValidator(String storageId,
                                                  String repositoryId,
                                                  String alias) throws IOException;

    boolean removeRepositoryArtifactCoordinateValidator(String storageId,
                                                        String repositoryId,
                                                        String alias) throws IOException;

    void setCorsAllowedOrigins(List<String> allowedOrigins) throws IOException;

    void setSmtpSettings(MutableSmtpConfiguration smtpConfiguration) throws IOException;

    void updateStorage(StorageDto storage) throws IOException;

    void updateStorageBasedir(StorageDto storage) throws IOException;

    /**
     * 按布局查询存储空间下的仓库列表
     *
     * @param storageId 存储空间id
     * @param layout    布局
     * @return 仓库列表
     */
    List<Repository> getRepositoriesWithLayout(String storageId, String layout);

    /**
     * 按类型查询存储空间下的仓库列表
     *
     * @param storageId      存储空间id
     * @param repositoryType 仓库类型
     * @return 仓库列表
     */
    List<Repository> getRepositoriesWithType(String storageId, String repositoryType);

    /**
     * 新增元数据设置
     *
     * @param mutableMetadataConfiguration 元数据配置参数
     * @throws IOException io异常
     */
    void addOrUpdateMetadataConfiguration(MutableMetadataConfiguration mutableMetadataConfiguration) throws IOException;

    /**
     * 删除元数据设置
     *
     * @param key 元数据key
     * @throws IOException io异常
     */
    void deleteMetadataConfig(String key) throws IOException;

    /**
     * 设置webhook配置信息
     *
     * @param webhookConfiguration 参数
     * @throws IOException io异常
     */
    void setWebhookConfiguration(Map<String, MutableWebhookConfiguration> webhookConfiguration) throws IOException;

    /**
     * 新增webhook配置信息
     *
     * @param mutableWebhookConfiguration 参数
     * @throws IOException io异常
     */
    void addWebhookConfiguration(MutableWebhookConfiguration mutableWebhookConfiguration) throws IOException;

    /**
     * 更新webhook配置信息
     *
     * @param mutableWebhookConfiguration 参数
     * @throws IOException io异常
     */
    void updateWebhookConfiguration(MutableWebhookConfiguration mutableWebhookConfiguration) throws IOException;

    /**
     * 删除webhook配置信息
     *
     * @param uuid 参数
     * @throws IOException io异常
     */
    void deleteWebhookConfiguration(String uuid) throws IOException;

    /**
     * 设置联邦仓库
     *
     * @param storageId                           存储空间
     * @param repositoryId                        仓库MC
     * @param mutableUnionRepositoryConfiguration 联邦仓库信息
     * @throws IOException io异常
     */
    void setUnionRepositoryConfiguration(String storageId,
                                         String repositoryId,
                                         MutableUnionRepositoryConfiguration mutableUnionRepositoryConfiguration) throws IOException;

    /**
     * 新增或者更新仓库
     * @param storageId     存储空间
     * @param repository    仓库
     */
    void addOrUpdateRepository(String storageId, RepositoryDto repository);
}
