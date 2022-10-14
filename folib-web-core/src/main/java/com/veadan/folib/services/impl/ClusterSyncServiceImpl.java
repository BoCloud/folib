package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.cluster.*;
import com.veadan.folib.controllers.cluster.dto.SyncRepositoryDto;
import com.veadan.folib.controllers.cluster.dto.SyncStorageDto;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import com.veadan.folib.mapper.ClusterDataSyncTaskMapper;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.math.BigInteger;
import java.util.UUID;

@Service
public class ClusterSyncServiceImpl implements ClusterSyncService {
    private static final Logger logger = LoggerFactory.getLogger(ClusterSyncServiceImpl.class);

    private final String SYCN_STORAGE_URI = "/api/configuration/cluster/syncStorage";
    private final String SYCN_REPOSITORY_URI = "/api/configuration/cluster/syncRepository";

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private ClusterProperties clusterProperties;

    @Autowired
    private FolibLockProperties ipProperties;

    @Autowired
    private ClusterDataSyncTaskMapper clusterDataSyncTaskMapper;

    @Override
    public void syncConfiguration() {
        logger.info("pjzy test sycnConfiguration");
    }

    @Override
    @Async("asyncStorageThreadPoolExecutor")
    public void syncStorage(StorageDto storageDto, String storageId, SyncStorageEnum syncStorageEnum) {
        // 向各个节点发送请求  判断全局配置的代理 判断是否是集群
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib sync storage");
        SyncStorageDto syncStorageDto = new SyncStorageDto(storageDto, storageId, syncStorageEnum);
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncStorage(storageId, syncStorageDto, nodeUrl, false);
        });

    }

    public ClusterSyncResultEnum handleSyncStorage(String storageId, SyncStorageDto syncStorageDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYCN_STORAGE_URI);
            response = target.request().post(Entity.entity(syncStorageDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync storage error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync storage error [{} ] {}", storageId, e.getMessage());
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncStorageDto),
                                SyncDataTypeEnum.STORAGE.getValue(),
                                SyncDataStatusEnum.WILL_EXECUTE_STATUS.getStatus()
                                , nodeUrl, BigInteger.valueOf(System.currentTimeMillis())
                        ));
            }
            return ClusterSyncResultEnum.FAIL;
        } finally {
            if (null != response) {
                response.close();
            }
            if (null != client) {
                client.close();
            }
        }
        return ClusterSyncResultEnum.SUCCESS;
    }

    public void addduledScheTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo) {
        clusterDataSyncTaskMapper.addTask(clusterDataSyncTaskPo);
    }


    @Override
    @Async("asyncRepositoryThreadPoolExecutor")
    public void syncRepository(String storageId, String repositoryId, RepositoryDto repository, SyncRepositoryEnum syncRepositoryEnum) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }

        logger.info("folib  sync repository");
        SyncRepositoryDto syncRepositoryDto = new SyncRepositoryDto(repository, storageId, repositoryId, syncRepositoryEnum);

        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncRepository(storageId, repositoryId, syncRepositoryDto, nodeUrl, false);
        });
    }

    public ClusterSyncResultEnum handleSyncRepository(String storageId, String repositoryId, SyncRepositoryDto syncRepositoryDto,
                                                      String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYCN_REPOSITORY_URI);
            response = target.request().post(Entity.entity(syncRepositoryDto, MediaType.APPLICATION_JSON));

            if (response.getStatus() > 210) {
                logger.error("sync repository error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync respository [{} {}] error {} ", storageId, repositoryId, e.getMessage());
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncRepositoryDto),
                                SyncDataTypeEnum.REPOSITORY.getValue(),
                                SyncDataStatusEnum.WILL_EXECUTE_STATUS.getStatus()
                                , nodeUrl, BigInteger.valueOf(System.currentTimeMillis())
                        ));
            }
            return ClusterSyncResultEnum.FAIL;
        } finally {
            if (null != response) {
                response.close();
            }

            if (null != client) {
                client.close();
            }
        }
        return ClusterSyncResultEnum.SUCCESS;
    }

    @Override
    public Boolean isNeedClusterSync() {

        if (!clusterProperties.getOpenFlag()) {
            logger.debug("cluster mode closed");
            return false;
        }
        if (StringUtils.isBlank(clusterProperties.getHostNode()) || clusterProperties.getHostNodeList().size() == 0) {
            logger.debug("cluster mode host node parameter error");
            return false;
        }
        return true;
    }

    @Override
    public Boolean clusterOpenFlag() {
        return clusterProperties.getOpenFlag();
    }

}
