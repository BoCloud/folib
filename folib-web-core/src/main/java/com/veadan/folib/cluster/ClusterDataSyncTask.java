package com.veadan.folib.cluster;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import com.veadan.folib.mapper.ClusterDataSyncTaskMapper;
import com.veadan.folib.services.ClusterSyncService;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;


@Component
@ConditionalOnProperty(name = "folib.cluster.openflag", havingValue = "true")
@EnableScheduling
public class ClusterDataSyncTask {
    private static final Logger logger = LoggerFactory.getLogger(ClusterDataSyncTask.class);

    @Autowired
    private FolibLockProperties ipProperties;

    @Autowired
    private ClusterDataSyncTaskMapper clusterDataSyncTaskMapper;

    @Autowired
    private ClusterSyncService clusterSyncService;

    private Boolean reExecuteFlag = false;

    /**
     * 集群模式下当前实例操作 同步有问题的实例数据
     */
    @Scheduled(cron = "0 0/1 * * * ? ")
    public void handleSyncData() {
        if (reExecuteFlag) {
            return;
        }
        reExecuteFlag = true;
        logger.debug("Start handle abnormal node data");

        // 查询 cluster_datasync_task
        List<ClusterDataSyncTaskPo> list = clusterDataSyncTaskMapper.getClusterDataSyncTaskList(
                SyncDataStatusEnum.WILL_EXECUTE_STATUS.getStatus(), ipProperties.getFolibLockIp());
        if (CollectionUtils.isEmpty(list)) {
            logger.debug("Handle data is empty");
            reExecuteFlag = false;
            return;
        }
        list.forEach(task -> {
            try {
                String url = task.getUrl();
                //同步STORAGE
                if (Objects.equals(SyncDataTypeEnum.STORAGE.getValue(), task.getTaskType())) {
                    SyncStorageDto storageDto = JSONObject.parseObject(task.getDataJson(),
                            SyncStorageDto.class);
                    logger.debug("Start sync storage data [{} {}]", storageDto.getStorageId(), url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncStorage(storageDto.getStorageId(), storageDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync data end [{} {}]", storageDto.getStorageId(), url);
                }
                //同步REPOSITORY
                if (Objects.equals(SyncDataTypeEnum.REPOSITORY.getValue(), task.getTaskType())) {

                    SyncRepositoryDto syncRepositoryDto = JSONObject.parseObject(task.getDataJson(),
                            SyncRepositoryDto.class);

                    logger.debug("Start sync repository data [{} {} {} ]",
                            syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId(), url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncRepository(syncRepositoryDto.getStorageId(),
                            syncRepositoryDto.getRepositoryId(), syncRepositoryDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync repository data end [{} {} {} ]",
                            syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId(), url);
                }
                //同步SECURITY_POLICY
                if (Objects.equals(SyncDataTypeEnum.SECURITY_POLICY.getValue(), task.getTaskType())) {

                    MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = JSONObject.parseObject(task.getDataJson(),
                            MutableSecurityPolicyConfiguration.class);

                    logger.debug("Start sync securityPolicyConfiguration data [{}]", url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncSecurityPolicyConfiguration(mutableSecurityPolicyConfiguration, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync securityPolicyConfiguration data end [{} ]", url);
                }
                //同步METADATA
                if (Objects.equals(SyncDataTypeEnum.METADATA.getValue(), task.getTaskType())) {

                    SyncMetadataDto syncMetadataDto = JSONObject.parseObject(task.getDataJson(),
                            SyncMetadataDto.class);

                    logger.debug("Start sync metadataConfiguration data [{}]", url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncMetadataConfiguration(syncMetadataDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync metadataConfiguration data end [{} ]", url);
                }
                // 同步仓库定时任务
                if (Objects.equals(SyncDataTypeEnum.REPOSITORY_JOB.getValue(), task.getTaskType())) {
                    SyncCronJobDto syncCronJobDto = JSONObject.parseObject(task.getDataJson(),
                            SyncCronJobDto.class);

                    logger.debug("Start sync cronJob data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncCronJob(syncCronJobDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync cronJob data end [{} ]", url);
                }
                // 同步授权配置信息
                if (Objects.equals(SyncDataTypeEnum.AUTHORIZATION.getValue(), task.getTaskType())) {
                    SyncAuthorizationDto syncAuthorizationDto = JSONObject.parseObject(task.getDataJson(),
                            SyncAuthorizationDto.class);

                    logger.debug("Start sync authorization data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncAuthorization(syncAuthorizationDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync authorization data end [{} ]", url);
                }
                // 同步webhook配置信息
                if (Objects.equals(SyncDataTypeEnum.WEBHOOK.getValue(), task.getTaskType())) {
                    SyncWebhookDto syncWebhookDto = JSONObject.parseObject(task.getDataJson(),
                            SyncWebhookDto.class);

                    logger.debug("Start sync webhook data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncWebhookConfiguration(syncWebhookDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync webhook data end [{} ]", url);
                }
                // 同步分发配置
                if (Objects.equals(SyncDataTypeEnum.CLUSTER_DISPATCH.getValue(), task.getTaskType())) {
                    SyncClusterDispatchDto syncClusterDispatchDto = JSONObject.parseObject(task.getDataJson(), SyncClusterDispatchDto.class);
                    logger.debug("Start sync dispatch data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncClusterDispatch(syncClusterDispatchDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync dispatch data end [{} ]", url);
                }
                // 同步联邦仓库配置
                if (Objects.equals(SyncDataTypeEnum.UNION_REPOSITORY.getValue(), task.getTaskType())) {
                    SyncUnionRepositoryDto syncUnionRepositoryDto = JSONObject.parseObject(task.getDataJson(), SyncUnionRepositoryDto.class);
                    logger.debug("Start sync unionRepository data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncUnionRepositoryConfiguration(syncUnionRepositoryDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync unionRepository data end [{} ]", url);
                }
                // 同步全局配置
                if (Objects.equals(SyncDataTypeEnum.SERVER_SETTINGS.getValue(), task.getTaskType())) {
                    SyncServerSettingsDto syncServerSettingsDto = JSONObject.parseObject(task.getDataJson(), SyncServerSettingsDto.class);
                    logger.debug("Start sync serverSettings data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncServerSettingsConfiguration(syncServerSettingsDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync serverSettings data end [{} ]", url);
                }
                // 同步LDAP配置
                if (Objects.equals(SyncDataTypeEnum.LDAP.getValue(), task.getTaskType())) {
                    SyncLdapDto syncLdapDto = JSONObject.parseObject(task.getDataJson(), SyncLdapDto.class);
                    logger.debug("Start sync Ldap data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncLdapConfiguration(syncLdapDto, url, true);
                    isSuccess(syncResult, task);
                    logger.debug("Sync Ldap data end [{} ]", url);
                }
            } catch (Exception e) {
                logger.error("Error [{}]", ExceptionUtils.getStackTrace(e));
            }
        });
        reExecuteFlag = false;
        logger.debug("handle abnormal node data end ");
    }

    private void isSuccess(ClusterSyncResultEnum syncResult, ClusterDataSyncTaskPo task) {
        if (syncResult.getCode() == 200) {
            // 更新task 状态
            task.setStatus(SyncDataStatusEnum.COMPLETE_STATUS.getStatus());
            clusterDataSyncTaskMapper.updateTask(task);
            logger.debug("Sync success");
        }
    }

}
