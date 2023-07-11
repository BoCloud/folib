package com.veadan.folib.controllers.cluster;


import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.cluster.SyncAuthorizationEnum;
import com.veadan.folib.cluster.SyncMetadataEnum;
import com.veadan.folib.cluster.SyncUnionRepositoryEnum;
import com.veadan.folib.cluster.SyncWebhookEnum;
import com.veadan.folib.configuration.MutableSecurityPolicyConfiguration;
import com.veadan.folib.configuration.MutableWebhookConfiguration;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ClusterDispatchManagementService;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import io.swagger.annotations.Api;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.nio.file.Files;
import java.util.Objects;


@RestController
//@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/cluster/")
@Api(value = "/api/configuration/cluster/")
public class FolibClusterSyncController extends BaseController {
    private static final Logger logger = LoggerFactory.getLogger(FolibClusterSyncController.class);

    @Autowired
    private RepositoryManagementService repositoryManagementService;

    @Autowired
    private StorageManagementService storageManagementService;

    @Autowired
    private CronTaskConfigurationService cronTaskConfigurationService;

    @Autowired
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Autowired
    private AuthorizationConfigService authorizationConfigService;

    @Autowired
    private ClusterDispatchManagementService clusterDispatchManagementService;

    @PostMapping("syncStorage")
    public ResponseEntity syncStorage(@RequestBody SyncStorageDto syncStorageDto) {
        try {
            if (syncStorageDto.getSycnStorageEnum().getType() == 1) {
                storageManagementService.createStorage(syncStorageDto.getStorageDto());
                logger.info("sync create storage [{}] success", syncStorageDto.getStorageId());
            } else if (syncStorageDto.getSycnStorageEnum().getType() == 2) {
                storageManagementService.updateStorage(syncStorageDto.getStorageDto());
                logger.info("sync update storage [{}] success", syncStorageDto.getStorageId());
            } else if (syncStorageDto.getSycnStorageEnum().getType() == 3) {
                if (syncStorageDto.getDeleteForceFlag()) {
                    final Storage storage = configurationManagementService.getConfiguration().getStorage(syncStorageDto.getStorageId());
                    if (Objects.nonNull(storage) && MapUtils.isNotEmpty(storage.getRepositories())) {
                        storageManagementService.removeStorage(syncStorageDto.getStorageId());
                    }
                    repositoryEventListenerRegistry.dispatchRepoDelteAllToCronJobDeleteEvent(syncStorageDto.getStorageId(), "");
                }
                configurationManagementService.removeStorage(syncStorageDto.getStorageId());
                logger.info("sync remove storage [{}] success", syncStorageDto.getStorageId());
            }
        } catch (Exception e) {
            logger.error("sync storage error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync storage ok");
    }

    @PostMapping("syncSecurityPolicyConfiguration")
    public ResponseEntity syncSecurityPolicyConfiguration(@RequestBody MutableSecurityPolicyConfiguration mutableSecurityPolicyConfiguration) {
        try {
            configurationManagementService.saveOrUpdateSecurityPolicy(mutableSecurityPolicyConfiguration);
            logger.info("sync securityPolicyConfiguration success");
        } catch (Exception e) {
            logger.error("sync securityPolicyConfiguration error {}", e.getMessage());
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync securityPolicyConfiguration ok");
    }

    @PostMapping("syncMetadataConfiguration")
    public ResponseEntity syncMetadataConfiguration(@RequestBody SyncMetadataDto syncMetadataDto) {
        try {
            if (SyncMetadataEnum.ADD_OR_UPDATE.getType().equals(syncMetadataDto.getSyncMetadataEnum().getType())) {
                configurationManagementService.addOrUpdateMetadataConfiguration(syncMetadataDto.getMutableMetadataConfiguration());
            } else if (SyncMetadataEnum.DELETE.getType().equals(syncMetadataDto.getSyncMetadataEnum().getType())) {
                configurationManagementService.deleteMetadataConfig(syncMetadataDto.getMutableMetadataConfiguration().getKey());
            }
            logger.info("sync syncMetadataConfiguration success");
        } catch (Exception e) {
            logger.error("sync syncMetadataConfiguration error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync syncMetadataConfiguration ok");
    }

    @PostMapping("syncRepository")
    public ResponseEntity syncRepository(@RequestBody SyncRepositoryDto syncRepositoryDto) {
        try {
            if (syncRepositoryDto.getSycnRepositoryEnum().getType() == 1) {
                configurationManagementService.saveRepository(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryDto());
                logger.info("sync save repository [{}] [{}] success", syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId());
            } else if (syncRepositoryDto.getSycnRepositoryEnum().getType() == 2) {
                if (syncRepositoryDto.getDeleteForceFlag()) {
                    Repository repository = getRepository(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId());
                    if (Objects.nonNull(repository)) {
                        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
                        if (Objects.nonNull(repositoryPath) && Files.exists(repositoryPath)) {
                            repositoryManagementService.removeRepository(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId());
                        }
                    }
                    repositoryEventListenerRegistry.
                            dispatchRepoDelteToCronJobDeleteEvent(syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId());
                }
                configurationManagementService.removeRepository(syncRepositoryDto.getStorageId(),
                        syncRepositoryDto.getRepositoryId());
                logger.info("sync remove repository [{}] [{}] success", syncRepositoryDto.getStorageId(), syncRepositoryDto.getRepositoryId());
            }
        } catch (Exception e) {
            logger.error("sync repository error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync repository ok");
    }

    /**
     * 同步定时任务
     *
     * @param syncCronJobDto 定时任务dto
     * @return 返回结果
     */
    @PostMapping("syncRepositoryJob")
    public ResponseEntity syncCronJob(@RequestBody SyncCronJobDto syncCronJobDto) {
        try {
            if (syncCronJobDto.getSyncCornJobEnum().getType() == 1) {
                cronTaskConfigurationService.saveConfiguration(syncCronJobDto.getConfigurationDto());
                logger.info("sync  save cron job success");
            } else if (syncCronJobDto.getSyncCornJobEnum().getType() == 2) {
                cronTaskConfigurationService.deleteConfiguration(syncCronJobDto.getConfigurationDto().getUuid());
                logger.info("sync delete cron job success");
            }
        } catch (Exception e) {
            logger.error("sync cronJob error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync syncCronJob ok");
    }

    @PostMapping("syncClusterDispatch")
    public ResponseEntity syncClusterDispatch(@RequestBody SyncClusterDispatchDto syncClusterDispatchDto) {
        try {
            if (syncClusterDispatchDto.getSyncClusterDispatchEnum().getType() == 1) {
                clusterDispatchManagementService.createClusterNode(syncClusterDispatchDto.getNodeDto());
                logger.info("sycn  save or update dispatch config success");
            } else if (syncClusterDispatchDto.getSyncClusterDispatchEnum().getType() == 2) {
                clusterDispatchManagementService.deleteClusterNode(syncClusterDispatchDto.getNodeDto());
                logger.info("sycn  delete dispatch config success");
            }
        } catch (Exception e) {
            e.printStackTrace();
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync syncCronJob ok");
    }

    /**
     * 同步授权配置信息
     *
     * @param syncAuthorizationDto 授权配置dto
     * @return 返回结果
     */
    @PostMapping("syncAuthorization")
    public ResponseEntity syncAuthorization(@RequestBody SyncAuthorizationDto syncAuthorizationDto) {
        try {
            if (SyncAuthorizationEnum.UPDATE.getType().equals(syncAuthorizationDto.getSyncAuthorizationEnum().getType())) {
                authorizationConfigService.setAuthorizationConfig(syncAuthorizationDto.getAuthorizationConfigDto());
                logger.info("sync update authorization success");
            }
        } catch (Exception e) {
            logger.error("sync authorization error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync authorization ok");
    }

    /**
     * 同步webhook配置
     *
     * @param syncWebhookDto webhook配置
     * @return 返回结果
     */
    @PostMapping("syncWebhook")
    public ResponseEntity syncWebhook(@RequestBody SyncWebhookDto syncWebhookDto) {
        try {
            MutableWebhookConfiguration mutableWebhookConfiguration = MutableWebhookConfiguration.builder().build();
            BeanUtils.copyProperties(syncWebhookDto.getWebhookConfigurationForm(), mutableWebhookConfiguration);
            if (SyncWebhookEnum.ADD.getType().equals(syncWebhookDto.getSyncWebhookEnum().getType())) {
                configurationManagementService.addWebhookConfiguration(mutableWebhookConfiguration);
                logger.info("sync add webhook success");
            } else if (SyncWebhookEnum.UPDATE.getType().equals(syncWebhookDto.getSyncWebhookEnum().getType())) {
                configurationManagementService.updateWebhookConfiguration(mutableWebhookConfiguration);
                logger.info("sync update webhook success");
            } else if (SyncWebhookEnum.DELETE.getType().equals(syncWebhookDto.getSyncWebhookEnum().getType())) {
                configurationManagementService.deleteWebhookConfiguration(syncWebhookDto.getWebhookConfigurationForm().getUuid());
                logger.info("sync delete webhook success");
            }
        } catch (Exception e) {
            logger.error("sync webhook error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync webhook ok");
    }

    /**
     * 同步联邦仓库配置
     *
     * @param syncUnionRepositoryDto 联邦仓库配置
     * @return 返回结果
     */
    @PostMapping("syncUnionRepository")
    public ResponseEntity syncUnionRepository(@RequestBody SyncUnionRepositoryDto syncUnionRepositoryDto) {
        try {

            if (SyncUnionRepositoryEnum.ADD_OR_UPDATE.getType().equals(syncUnionRepositoryDto.getSyncUnionRepositoryEnum().getType())) {
                configurationManagementService.setUnionRepositoryConfiguration(syncUnionRepositoryDto.getStorageId(), syncUnionRepositoryDto.getRepositoryId(), syncUnionRepositoryDto.getUnionRepositoryConfigurationForm().getMutableUnionRepositoryConfiguration());
                logger.info("sync add or update unionRepository success");
            }
        } catch (Exception e) {
            logger.error("sync unionRepository error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("sync unionRepository ok");
    }
}
