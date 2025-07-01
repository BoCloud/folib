package com.veadan.folib.services;

import com.veadan.folib.cluster.ClusterSyncResultEnum;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;

import java.util.Set;

public interface ClusterSyncService {

    void syncConfiguration();

    void syncStorage(SyncStorageDto syncStorageDto);

    void syncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration);

    void syncMetadataConfiguration(SyncMetadataDto syncMetadataDto);

    void syncRepository(SyncRepositoryDto syncRepositoryDto);

    void syncCronJob(SyncCronJobDto syncCronJobDto);

    /**
     * 同步授权配置信息
     *
     * @param syncAuthorizationDto 授权配置信息
     */
    void syncAuthorization(SyncAuthorizationDto syncAuthorizationDto);

    /**
     * 同步webhook配置
     *
     * @param syncWebhookDto webhook配置
     */
    void syncWebhookConfiguration(SyncWebhookDto syncWebhookDto);

    /**
     * 同步联邦仓库配置
     *
     * @param syncUnionRepositoryDto 联邦仓库配置
     */
    void syncUnionRepositoryConfiguration(SyncUnionRepositoryDto syncUnionRepositoryDto);

    /**
     * 同步全局配置
     *
     * @param syncServerSettingsDto 全局配置
     */
    void syncServerSettings(SyncServerSettingsDto syncServerSettingsDto);

    /**
     * 同步LDAP配置
     *
     * @param syncLdapDto LDAP配置
     */
    void syncLdapConfiguration(SyncLdapDto syncLdapDto);

    Boolean isNeedClusterSync();

    Boolean clusterOpenFlag();

    ClusterSyncResultEnum handleSyncStorage(String storageId, SyncStorageDto syncStorageDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncRepository(String storageId, String repositoryId, SyncRepositoryDto syncRepositoryDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncMetadataConfiguration(SyncMetadataDto syncMetadataDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncCronJob(SyncCronJobDto syncCronJobDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncAuthorization(SyncAuthorizationDto syncAuthorizationDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncWebhookConfiguration(SyncWebhookDto syncWebhookDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncUnionRepositoryConfiguration(SyncUnionRepositoryDto syncUnionRepositoryDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncServerSettingsConfiguration(SyncServerSettingsDto syncServerSettingsDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncLdapConfiguration(SyncLdapDto syncLdapDto, String nodeUrl, Boolean isScheduled);

    void addScheduledTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

    Set<String> getHostNodeList();

    Set<String> getHostNodeListV2();
}
