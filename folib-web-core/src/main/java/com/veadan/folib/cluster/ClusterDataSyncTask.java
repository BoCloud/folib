package com.veadan.folib.cluster;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.entity.ClusterDataSyncTaskPo;
import com.veadan.folib.mapper.ClusterDataSyncTaskMapper;
import com.veadan.folib.services.ClusterSyncService;
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
        logger.info("start handle abnormal node data");

        // 查询 cluster_datasync_task
        List<ClusterDataSyncTaskPo> list = clusterDataSyncTaskMapper.getClusterDataSyncTaskList(
                SyncDataStatusEnum.WILL_EXECUTE_STATUS.getStatus(), ipProperties.getFolibLockIp());
        if (CollectionUtils.isEmpty(list)) {
            logger.info("handle data is empty");
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
                    logger.info("start sync storage data [{} {}]", storageDto.getStorageId(), url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncStorage(storageDto.getStorageId(), storageDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync data end [{} {}]", storageDto.getStorageId(), url);
                }
                //同步REPOSITORY
                if (Objects.equals(SyncDataTypeEnum.REPOSITORY.getValue(), task.getTaskType())) {

                    SyncRepositoryDto syncRepositoryDto = JSONObject.parseObject(task.getDataJson(),
                            SyncRepositoryDto.class);

                    logger.info("start sync repository data [{} {} {} ]",
                            syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId(), url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncRepository(syncRepositoryDto.getStorageId(),
                            syncRepositoryDto.getRepositoryId(), syncRepositoryDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync repository data end [{} {} {} ]",
                            syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId(), url);
                }
                //同步SECURITY_POLICY
                if (Objects.equals(SyncDataTypeEnum.SECURITY_POLICY.getValue(), task.getTaskType())) {

                    MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration = JSONObject.parseObject(task.getDataJson(),
                            MutableSecurityPolicyConfiguration.class);

                    logger.info("start sync securityPolicyConfiguration data [{}]", url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncSecurityPolicyConfiguration(mutableSecurityPolicyConfiguration, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync securityPolicyConfiguration data end [{} ]", url);
                }
                //同步METADATA
                if (Objects.equals(SyncDataTypeEnum.METADATA.getValue(), task.getTaskType())) {

                    SyncMetadataDto syncMetadataDto = JSONObject.parseObject(task.getDataJson(),
                            SyncMetadataDto.class);

                    logger.info("start sync metadataConfiguration data [{}]", url);

                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncMetadataConfiguration(syncMetadataDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync metadataConfiguration data end [{} ]", url);
                }
                // 同步仓库定时任务
                if (Objects.equals(SyncDataTypeEnum.REPOSITORY_JOB.getValue(), task.getTaskType())) {
                    SyncCronJobDto syncCronJobDto = JSONObject.parseObject(task.getDataJson(),
                            SyncCronJobDto.class);

                    logger.info("start sync cronJob data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncCronJob(syncCronJobDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync cronJob data end [{} ]", url);
                }
                // 同步授权配置信息
                if (Objects.equals(SyncDataTypeEnum.AUTHORIZATION.getValue(), task.getTaskType())) {
                    SyncAuthorizationDto syncAuthorizationDto = JSONObject.parseObject(task.getDataJson(),
                            SyncAuthorizationDto.class);

                    logger.info("start sync authorization data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncAuthorization(syncAuthorizationDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync authorization data end [{} ]", url);
                }
                // 同步webhook配置信息
                if (Objects.equals(SyncDataTypeEnum.WEBHOOK.getValue(), task.getTaskType())) {
                    SyncWebhookDto syncWebhookDto = JSONObject.parseObject(task.getDataJson(),
                            SyncWebhookDto.class);

                    logger.info("start sync webhook data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncWebhookConfiguration(syncWebhookDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync webhook data end [{} ]", url);
                }
                // 同步分发配置
                if (Objects.equals(SyncDataTypeEnum.CLUSTER_DISPATCH.getValue(), task.getTaskType())) {
                    SyncClusterDispatchDto syncClusterDispatchDto = JSONObject.parseObject(task.getDataJson(), SyncClusterDispatchDto.class);
                    logger.info("start sync dispatch data [{}]", url);
                    ClusterSyncResultEnum syncResult = clusterSyncService.handleSyncClusterDispatch(syncClusterDispatchDto, url, true);
                    isSuccess(syncResult, task);
                    logger.info("sync dispatch data end [{} ]", url);
                }
            } catch (Exception e) {
                logger.error("error {}", e.getMessage());
            }
        });
        reExecuteFlag = false;
        logger.info(" handle abnormal node data end ");
    }

    private void isSuccess(ClusterSyncResultEnum syncResult, ClusterDataSyncTaskPo task) {
        if (syncResult.getCode() == 200) {
            // 更新task 状态
            task.setStatus(SyncDataStatusEnum.COMPLETE_STATUS.getStatus());
            clusterDataSyncTaskMapper.updateTask(task);
            logger.info("sync success");
        }
    }

}
