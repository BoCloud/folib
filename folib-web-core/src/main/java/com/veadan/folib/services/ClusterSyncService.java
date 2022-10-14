package com.veadan.folib.services;

import com.veadan.folib.cluster.ClusterSyncResultEnum;
import com.veadan.folib.cluster.SyncRepositoryEnum;
import com.veadan.folib.cluster.SyncStorageEnum;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;

public interface ClusterSyncService {

    void syncConfiguration();

    void syncStorage(StorageDto storageDto,String storageId, SyncStorageEnum syncStorageEnum);

    void syncRepository(String storageId, String repositoryId ,RepositoryDto repository, SyncRepositoryEnum syncRepositoryEnum);

    Boolean isNeedClusterSync();

    Boolean clusterOpenFlag();

    ClusterSyncResultEnum handleSyncStorage(String storageId, SyncStorageDto syncStorageDto, String nodeUrl, Boolean isScheduled);

    ClusterSyncResultEnum handleSyncRepository(String storageId, String repositoryId, SyncRepositoryDto syncRepositoryDto, String nodeUrl, Boolean isScheduled);

    void addduledScheTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo);

}
