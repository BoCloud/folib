package com.veadan.folib.services;

import com.veadan.folib.client.MutableRemoteRepositoryRetryArtifactDownloadConfiguration;
import com.veadan.folib.configuration.*;
import com.veadan.folib.dispatch.ClusterDispatchNodeDto;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.routing.MutableRoutingRule;
import com.veadan.folib.storage.routing.MutableRoutingRules;

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

    void createClusterDispatchConfig(ClusterDispatchNodeDto clusterDispatchNodeDto) throws IOException;

    void removeClusterDispatchConfig(ClusterDispatchNodeDto clusterDispatchNodeDto) throws IOException;

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
     * 设置平台级别白名单
     *
     * @param whites 平台级别白名单
     * @throws IOException io异常
     */
    void setVulnerabilitiesWhites(String whites) throws IOException;

    /**
     * 设置平台级别黑名单
     *
     * @param blacks 平台级别黑名单
     * @throws IOException io异常
     */
    void setVulnerabilitiesBlacks(String blacks) throws IOException;

    /**
     * 添加平台级别白名单
     *
     * @param white 平台级别白名单
     * @throws IOException io异常
     */
    void addVulnerabilitiesWhite(String white) throws IOException;

    /**
     * 添加平台级别黑名单
     *
     * @param black 平台级别黑名单
     * @throws IOException io异常
     */
    void addVulnerabilitiesBlack(String black) throws IOException;

    /**
     * 删除平台级别白名单
     *
     * @param white 平台级别白名单
     * @throws IOException io异常
     */
    void removeVulnerabilitiesWhite(String white) throws IOException;

    /**
     * 删除平台级别黑名单
     *
     * @param black 平台级别黑名单
     * @throws IOException io异常
     */
    void removeVulnerabilitiesBlack(String black) throws IOException;

    /**
     * 获取平台级别白名单
     *
     * @return 平台级别白名单
     */
    Set<String> getVulnerabilityWhites();

    /**
     * 获取平台级别黑名单
     *
     * @return 平台级别黑名单
     */
    Set<String> getVulnerabilityBlacks();

    /**
     * 保存或者更新平台通知配置
     *
     * @param mutableSecurityPolicyConfiguration 参数
     * @throws IOException io异常
     */
    void saveOrUpdateNotify(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) throws IOException;

    /**
     * 保存或者更新平台阻断配置
     *
     * @param mutableSecurityPolicyConfiguration 参数
     * @throws IOException io异常
     */
    void saveOrUpdateBlock(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) throws IOException;

    /**
     * 保存或者更新平台安全策略配置
     *
     * @param mutableSecurityPolicyConfiguration 参数
     * @throws IOException io异常
     */
    void saveOrUpdateSecurityPolicy(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) throws IOException;

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
