package com.veadan.folib.services;

import com.veadan.folib.cluster.ClusterSyncResultEnum;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;

public interface ClusterSyncService {

    void syncConfiguration();

    void syncStorage(SyncStorageDto syncStorageDto);

    void syncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration);

    void syncMetadataConfiguration(SyncMetadataDto syncMetadataDto);

    void syncRepository(SyncRepositoryDto syncRepositoryDto);

    void syncCronJob(SyncCronJobDto syncCronJobDto);

    void syncClusterDispatch(SyncClusterDispatchDto syncClusterDispatchDto);

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

    Boolean isNeedClusterSync();

    Boolean clusterOpenFlag();

    ClusterSyncResultEnum handleSyncStorage(String storageId, SyncStorageDto syncStorageDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncRepository(String storageId, String repositoryId, SyncRepositoryDto syncRepositoryDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncMetadataConfiguration(SyncMetadataDto syncMetadataDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncCronJob(SyncCronJobDto syncCronJobDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncAuthorization(SyncAuthorizationDto syncAuthorizationDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncWebhookConfiguration(SyncWebhookDto syncWebhookDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncClusterDispatch(SyncClusterDispatchDto syncClusterDispatchDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncUnionRepositoryConfiguration(SyncUnionRepositoryDto syncUnionRepositoryDto, String nodeUrl, Boolean isScheduled);

    void addduledScheTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

}
