package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.beust.jcommander.internal.Sets;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.cluster.*;
import com.veadan.folib.components.node.NodeComponent;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import com.veadan.folib.entity.Dict;
import com.veadan.folib.enums.DictTypeEnum;
import com.veadan.folib.forms.node.CassandraClusterForm;
import com.veadan.folib.mapper.ClusterDataSyncTaskMapper;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ClusterSyncService;
import com.veadan.folib.services.DictService;
import org.apache.cassandra.tools.nodetool.HostStatWithPort;
import org.apache.cassandra.tools.nodetool.SetHostStatWithPort;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.math.BigInteger;
import java.util.*;

@Service
public class ClusterSyncServiceImpl implements ClusterSyncService {
    private static final Logger logger = LoggerFactory.getLogger(ClusterSyncServiceImpl.class);

    private final String SYNC_STORAGE_URI = "/api/configuration/cluster/syncStorage";
    private final String SYNC_REPOSITORY_URI = "/api/configuration/cluster/syncRepository";
    private final String SYNC_SECURITY_POLICY_URI = "/api/configuration/cluster/syncSecurityPolicyConfiguration";
    private final String SYNC_METADATA_URI = "/api/configuration/cluster/syncMetadataConfiguration";
    private final String SYNC_REPOSITORY_JOB = "/api/configuration/cluster/syncRepositoryJob";
    private final String SYNC_AUTHORIZATION = "/api/configuration/cluster/syncAuthorization";
    private final String SYNC_WEB_HOOK = "/api/configuration/cluster/syncWebhook";
    private final String SYNC_CLUSTER_DISPATCH_URI = "/api/configuration/cluster/syncClusterDispatch";
    private final String SYNC_UNION_REPOSITORY_URI = "/api/configuration/cluster/syncUnionRepository";
    private final String SYNC_SERVER_SETTINGS = "/api/configuration/cluster/syncServerSettings";
    private final String SYNC_LDAP = "/api/configuration/cluster/syncLdap";

    @Autowired
    private ProxyRepositoryConnectionPoolConfigurationService clientPool;

    @Autowired
    private ClusterProperties clusterProperties;

    @Autowired
    private FolibLockProperties ipProperties;

    @Autowired
    private ClusterDataSyncTaskMapper clusterDataSyncTaskMapper;

    @Autowired
    private NodeComponent nodeComponent;

    @Inject
    private PropertiesBooter propertiesBooter;

    @Autowired
    @Lazy
    private DictService dictService;

    @Inject
    private ObjectMapper objectMapper;

    @Override
    public void syncConfiguration() {
        logger.info("pjzy test sycnConfiguration");
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncStorage(SyncStorageDto syncStorageDto) {
        // 向各个节点发送请求  判断全局配置的代理 判断是否是集群
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync storage");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncStorage(syncStorageDto.getStorageId(), syncStorageDto, nodeUrl, false);
        });

    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncSecurityPolicyConfiguration(MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync securityPolicyConfiguration");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncSecurityPolicyConfiguration(mutableSecurityPolicyConfiguration, nodeUrl, false);
        });
    }

    @Override
    public Set<String> getHostNodeList() {
        Set<String> nodeSet = Sets.newLinkedHashSet();
        try {
            CassandraClusterForm cassandraClusterForm = nodeComponent.cassandraClusterInfo();
            SetHostStatWithPort statWithPorts = null;
            Iterator<HostStatWithPort> hostStatWithPortIterator = null;
            HostStatWithPort hostStatWithPort = null;
            String node = "";
            for (Map.Entry<String, SetHostStatWithPort> entry : cassandraClusterForm.getDcsMap().entrySet()) {
                statWithPorts = entry.getValue();
                if (Objects.nonNull(statWithPorts)) {
                    hostStatWithPortIterator = statWithPorts.iterator();
                    while (hostStatWithPortIterator.hasNext()) {
                        hostStatWithPort = hostStatWithPortIterator.next();
                        if (Objects.isNull(hostStatWithPort)) {
                            continue;
                        }
                        if (cassandraClusterForm.getEndpoint().equals(hostStatWithPort.endpointWithPort.getHostAddressAndPort())) {
                            //当前节点，跳过
                            logger.info("集群节点为 [{}] 当前节点为 [{}]，跳过同步配置", hostStatWithPort.endpointWithPort.getHostAddressAndPort(), cassandraClusterForm.getEndpoint());
                            continue;
                        }
                        node = String.format("%s%s%s%s", "http://", hostStatWithPort.endpoint.getHostAddress(), ":", propertiesBooter.getPort());
                        nodeSet.add(node);
                        logger.info("集群节点为 [{}]，加入到节点列表 [{}]", node, String.join(",", nodeSet));
                    }
                }
            }
            logger.info("集群节点列表为 [{}]", String.join(",", nodeSet));
            return nodeSet;
        } catch (Exception ex) {
            logger.error("Get host node list error {}", ExceptionUtils.getStackTrace(ex));
        }
        return Collections.emptySet();
    }

    @Override
    public Set<String> getHostNodeListV2() {
        List<Dict> dictList = dictService.selectDict(Dict.builder().dictType(DictTypeEnum.CLUSTER_NODES.getType()).build());
        if (CollectionUtils.isEmpty(dictList)) {
            return Collections.emptySet();
        }
        Set<String> nodeSet = Sets.newLinkedHashSet();
        String node = "", currentNode = ipProperties.getFolibLockIp();
        for (Dict dict : dictList) {
            node = dict.getDictKey();
            if (StringUtils.isBlank(node)) {
                continue;
            }
            if (currentNode.equalsIgnoreCase(node)) {
                logger.info("集群节点为 [{}] 当前节点为 [{}]，跳过同步配置", node, currentNode);
                continue;
            }
            node = String.format("%s%s%s%s", "http://", node, ":", propertiesBooter.getPort());
            nodeSet.add(node);
            logger.info("集群节点为 [{}]，加入到节点列表 [{}]", node, String.join(",", nodeSet));
        }
        logger.info("集群节点列表为 [{}]", String.join(",", nodeSet));
        return nodeSet;
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncMetadataConfiguration(SyncMetadataDto syncMetadataDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync metadataConfiguration");
        getHostNodeListV2().forEach(nodeUrl -> {
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
            if (response.getStatus() != 200) {
                logger.error("Sync storage error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync storage error [{} ] {}", storageId, ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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
    public void addScheduledTask(ClusterDataSyncTaskPo clusterDataSyncTaskPo) {
        clusterDataSyncTaskMapper.addTask(clusterDataSyncTaskPo);
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncRepository(SyncRepositoryDto syncRepositoryDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }

        logger.info("Sync repository");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncRepository(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId(),
                    syncRepositoryDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncCronJob(SyncCronJobDto syncCronJobDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync cron job");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncCronJob(syncCronJobDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncClusterDispatch(SyncClusterDispatchDto syncClusterDispatchDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync cluster dispatch job");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncClusterDispatch(syncClusterDispatchDto, nodeUrl, false);
        });
    }

    @Override
    public ClusterSyncResultEnum handleSyncClusterDispatch(SyncClusterDispatchDto syncClusterDispatchDto,
                                                           String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        String clusterEnName = syncClusterDispatchDto.getNodeDto().getClusterEnName();
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_CLUSTER_DISPATCH_URI);
            response = target.request().post(Entity.entity(syncClusterDispatchDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                logger.error("Sync cluster dispatch error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync cluster dispatch [{}] error {} ", clusterEnName, e.getMessage());
            if (!isScheduled) {
                addScheduledTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncClusterDispatchDto),
                                SyncDataTypeEnum.CLUSTER_DISPATCH.getValue(),
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
    public ClusterSyncResultEnum handleSyncUnionRepositoryConfiguration(SyncUnionRepositoryDto syncUnionRepositoryDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_UNION_REPOSITORY_URI);
            response = target.request().post(Entity.entity(syncUnionRepositoryDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                logger.error("Sync handleSyncUnionRepositoryConfiguration error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync handleSyncUnionRepositoryConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncUnionRepositoryDto),
                                SyncDataTypeEnum.UNION_REPOSITORY.getValue(),
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
    @Async("asyncConfigThreadPoolExecutor")
    public void syncAuthorization(SyncAuthorizationDto syncAuthorizationDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync authorization");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncAuthorization(syncAuthorizationDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncWebhookConfiguration(SyncWebhookDto syncWebhookDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync Webhook");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncWebhookConfiguration(syncWebhookDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncUnionRepositoryConfiguration(SyncUnionRepositoryDto syncUnionRepositoryDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync unionRepository job");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncUnionRepositoryConfiguration(syncUnionRepositoryDto, nodeUrl, false);
        });
    }

    @Override
    @Async("asyncConfigThreadPoolExecutor")
    public void syncServerSettings(SyncServerSettingsDto syncServerSettingsDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync serverSettings job");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncServerSettingsConfiguration(syncServerSettingsDto, nodeUrl, false);
        });
    }

    @Override
    public void syncLdapConfiguration(SyncLdapDto syncLdapDto) {
        if (!isNeedClusterSync()) {
            logger.info("Cluster mode not opened");
            return;
        }
        logger.info("Sync Ldap job");
        getHostNodeListV2().forEach(nodeUrl -> {
            handleSyncLdapConfiguration(syncLdapDto, nodeUrl, false);
        });
    }

    @Override
    public ClusterSyncResultEnum handleSyncCronJob(SyncCronJobDto syncCronJobDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        String storageId = syncCronJobDto.getConfigurationDto().getProperty("storageId");
        String repositoryId = syncCronJobDto.getConfigurationDto().getProperty("repositoryId");
        logger.info("Start handleSyncCronJob {}", JSON.toJSONString(syncCronJobDto));
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_REPOSITORY_JOB);
            response = target.request().post(Entity.entity(syncCronJobDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                logger.error("Sync cronJob error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync cronJob [{} {}] error {} ", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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

            if (response.getStatus() != 200) {
                logger.error("Sync repository error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync repository [{} {}] error {} ", storageId, repositoryId, ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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
            if (response.getStatus() != 200) {
                logger.error("Sync securityPolicyConfiguration error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync securityPolicyConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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
            if (response.getStatus() != 200) {
                logger.error("Sync handleSyncMetadataConfiguration error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync handleSyncMetadataConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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
            if (response.getStatus() != 200) {
                logger.error("Sync handleSyncAuthorization error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync handleSyncAuthorization error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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
            WebTarget target = client.target(nodeUrl + SYNC_WEB_HOOK);
            response = target.request().post(Entity.entity(syncWebhookDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                logger.error("Sync handleSyncWebhookConfiguration error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync handleSyncWebhookConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
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
    public ClusterSyncResultEnum handleSyncServerSettingsConfiguration(SyncServerSettingsDto syncServerSettingsDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        try {
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_SERVER_SETTINGS);
            response = target.request().post(Entity.entity(syncServerSettingsDto, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                logger.error("Sync handleServerSettingsConfiguration error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync handleServerSettingsConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                JSON.toJSONString(syncServerSettingsDto),
                                SyncDataTypeEnum.SERVER_SETTINGS.getValue(),
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
    public ClusterSyncResultEnum handleSyncLdapConfiguration(SyncLdapDto syncLdapDto, String nodeUrl, Boolean isScheduled) {
        Response response = null;
        Client client = null;
        String data = "";
        try {
            data = objectMapper.writeValueAsString(syncLdapDto);
            client = clientPool.getRestClient();
            WebTarget target = client.target(nodeUrl + SYNC_LDAP);
            response = target.request().post(Entity.entity(data, MediaType.APPLICATION_JSON));
            if (response.getStatus() != 200) {
                logger.error("Sync handleSyncLdapConfiguration error {} {}", nodeUrl, JSONObject.toJSONString(response));
                throw new RuntimeException("Failed with HTTP error code : " + response.getStatus());
            }
        } catch (Exception e) {
            logger.error("Sync handleSyncLdapConfiguration error {} ", ExceptionUtils.getStackTrace(e));
            if (!isScheduled) {
                addScheduledTask(
                        new ClusterDataSyncTaskPo(UUID.randomUUID().toString(),
                                ipProperties.getFolibLockIp(),
                                data,
                                SyncDataTypeEnum.LDAP.getValue(),
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
            logger.info("Cluster mode not opened");
            return false;
        }
        if (getHostNodeListV2().size() == 0) {
            logger.info("No valid node is found in cluster mode");
            return false;
        }
        return true;
    }

    @Override
    public Boolean clusterOpenFlag() {
        return clusterProperties.getOpenFlag();
    }

}
