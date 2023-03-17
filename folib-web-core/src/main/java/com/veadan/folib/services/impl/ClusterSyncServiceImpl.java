package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.veadan.folib.authorization.dto.AuthorizationConfigDto;
import com.veadan.folib.cluster.*;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import com.veadan.folib.mapper.ClusterDataSyncTaskMapper;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
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

    private final String SYNC_STORAGE_URI = "/api/configuration/cluster/syncStorage";
    private final String SYNC_REPOSITORY_URI = "/api/configuration/cluster/syncRepository";
    private final String SYNC_SECURITY_POLICY_URI = "/api/configuration/cluster/syncSecurityPolicyConfiguration";
    private final String SYNC_METADATA_URI = "/api/configuration/cluster/syncMetadataConfiguration";
    private final String SYNC_REPOSITORY_JOB = "/api/configuration/cluster/syncRepositoryJob";
    private final String SYNC_AUTHORIZATION = "/api/configuration/cluster/syncAuthorization";
    private final String SYNC_WEBHOOK = "/api/configuration/cluster/syncWebhook";

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
        logger.info("pjzy test syncConfiguration");
    }

    @Override
    @Async("asyncStorageThreadPoolExecutor")
    public void syncStorage(SyncStorageDto syncStorageDto) {
        // 向各个节点发送请求  判断全局配置的代理 判断是否是集群
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib sync storage");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncStorage(syncStorageDto.getStorageId(), syncStorageDto, nodeUrl, false);
        });

    }

    @Override
    @Async("asyncSecurityPolicyConfigurationThreadPoolExecutor")
    public void syncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib sync securityPolicyConfiguration");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncSecurityPolicyConfiguration(mutableSecurityPolicyConfiguration, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncMetadataConfiguration(SyncMetadataDto syncMetadataDto) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib sync metadataConfiguration");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncMetadataConfiguration(syncMetadataDto, nodeUrl, false);
        });
    }

    @Override
    public ClusterSyncResultEnum handleSyncStorage(String storageId, SyncStorageDto syncStorageDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_STORAGE_URI);
            response = target.request().post(Entity.entity(syncStorageDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync storage error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync storage error [{} ] {}", storageId, ExceptionUtils.getStackTrace(e));
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

    @Override
    public void addduledScheTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo) {
        clusterDataSyncTaskMapper.addTask(clusterDataSyncTaskPo);
    }


    @Override
    @Async("asyncRepositoryThreadPoolExecutor")
    public void syncRepository(SyncRepositoryDto syncRepositoryDto) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }

        logger.info("folib  sync repository");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncRepository(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId(),
                    syncRepositoryDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncCronJobThreadPoolExecutor")
    public void syncCronJob(SyncCronJobDto syncCronJobDto) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib  sync cron job");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncCronJob(syncCronJobDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncAuthorization(SyncAuthorizationDto syncAuthorizationDtoo) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib  sync authorization");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncAuthorization(syncAuthorizationDtoo, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncWebhookConfiguration(SyncWebhookDto syncWebhookDto) {
        if (!isNeedClusterSync()) {
            logger.info("cluster mode not opened");
            return;
        }
        logger.info("folib  sync Webhook");
        clusterProperties.getHostNodeList().forEach(nodeUrl -> {
            handleSyncWebhookConfiguration(syncWebhookDto, nodeUrl, false);
        });
    }

    @Override
    public ClusterSyncResultEnum handleSyncCronJob(SyncCronJobDto syncCronJobDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        String storageId = syncCronJobDto.getConfigurationDto().getProperty("storageId");
        String repositoryId = syncCronJobDto.getConfigurationDto().getProperty("repositoryId");
        logger.info("start handleSyncCronJob {}", JSON.toJSONString(syncCronJobDto));
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_REPOSITORY_JOB);
            response = target.request().post(Entity.entity(syncCronJobDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync CronJob error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync CronJob [{} {}] error {} ",storageId , repositoryId, ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncCronJobDto),
                                SyncDataTypeEnum.REPOSITORY_JOB.getValue(),
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
    public ClusterSyncResultEnum handleSyncRepository(String storageId, String repositoryId, SyncRepositoryDto syncRepositoryDto,
                                                      String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_REPOSITORY_URI);
            response = target.request().post(Entity.entity(syncRepositoryDto, MediaType.APPLICATION_JSON));

            if (response.getStatus() > 210) {
                logger.error("sync repository error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync respository [{} {}] error {} ", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
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
    public ClusterSyncResultEnum handleSyncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_SECURITY_POLICY_URI);
            response = target.request().post(Entity.entity(mutableSecurityPolicyConfiguration, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync securityPolicyConfiguration error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync securityPolicyConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(mutableSecurityPolicyConfiguration),
                                SyncDataTypeEnum.SECURITY_POLICY.getValue(),
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
    public ClusterSyncResultEnum handleSyncMetadataConfiguration(SyncMetadataDto syncMetadataDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_METADATA_URI);
            response = target.request().post(Entity.entity(syncMetadataDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync handleSyncMetadataConfiguration error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync handleSyncMetadataConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncMetadataDto),
                                SyncDataTypeEnum.METADATA.getValue(),
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
    public ClusterSyncResultEnum handleSyncAuthorization(SyncAuthorizationDto syncAuthorizationDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_AUTHORIZATION);
            response = target.request().post(Entity.entity(syncAuthorizationDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync handleSyncAuthorization error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync handleSyncAuthorization error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncAuthorizationDto),
                                SyncDataTypeEnum.AUTHORIZATION.getValue(),
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
    public ClusterSyncResultEnum handleSyncWebhookConfiguration(SyncWebhookDto syncWebhookDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_WEBHOOK);
            response = target.request().post(Entity.entity(syncWebhookDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() > 210) {
                logger.error("sync handleSyncWebhookConfiguration error {}", nodeUrl);
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("sync handleSyncWebhookConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addduledScheTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncWebhookDto),
                                SyncDataTypeEnum.WEBHOOK.getValue(),
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
