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

    Boolean isNeedClusterSync();

    Boolean clusterOpenFlag();

    ClusterSyncResultEnum handleSyncStorage(String storageId, SyncStorageDto syncStorageDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncRepository(String storageId, String repositoryId, SyncRepositoryDto syncRepositoryDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncMetadataConfiguration(SyncMetadataDto syncMetadataDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncCronJob(SyncCronJobDto syncCronJobDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncClusterDispatch(SyncClusterDispatchDto syncClusterDispatchDto, String nodeUrl, Boolean isScheduled);

    void addduledScheTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

}
