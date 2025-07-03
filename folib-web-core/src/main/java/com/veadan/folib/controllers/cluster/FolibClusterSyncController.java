package com.veadan.folib.controllers.cluster;


import com.veadan.folib.authorization.service.AuthorizationConfigService;
import com.veadan.folib.cluster.*;
import com.veadan.folib.components.common.CommonComponent;
import com.veadan.folib.configuration.MutableWebhookConfiguration;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.controllers.cluster.dto.*;
import com.veadan.folib.job.cron.services.CronTaskConfigurationService;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.services.RepositoryManagementService;
import com.veadan.folib.services.StorageManagementService;
import com.veadan.folib.storage.Storage;
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

import java.util.Objects;


@RestController
//@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/configuration/cluster/")
@Api(description = "集群配置管理", tags = "集群配置管理")
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
    private CommonComponent commonComponent;

    @PostMapping("syncStorage")
    public ResponseEntity syncStorage(@RequestBody SyncStorageDto syncStorageDto) {
        try {
            if (syncStorageDto.getSyncStorageEnum().getType() == 1) {
                storageManagementService.createStorage(syncStorageDto.getStorageDto());
                logger.info("Sync create storage [{}] success", syncStorageDto.getStorageId());
            } else if (syncStorageDto.getSyncStorageEnum().getType() == 2) {
                storageManagementService.updateStorage(syncStorageDto.getStorageDto());
                logger.info("Sync update storage [{}] success", syncStorageDto.getStorageId());
            } else if (syncStorageDto.getSyncStorageEnum().getType() == 3) {
                if (syncStorageDto.getDeleteForceFlag()) {
                    final Storage storage = configurationManagementService.getConfiguration().getStorage(syncStorageDto.getStorageId());
                    if (Objects.nonNull(storage) && MapUtils.isNotEmpty(storage.getRepositories())) {
                        storageManagementService.removeStorage(syncStorageDto.getStorageId());
                    }
                    repositoryEventListenerRegistry.dispatchRepoDelteAllToCronJobDeleteEvent(syncStorageDto.getStorageId(), "");
                }
                configurationManagementService.removeStorage(syncStorageDto.getStorageId());
                logger.info("Sync remove storage [{}] success", syncStorageDto.getStorageId());
            }
        } catch (Exception e) {
            logger.error("Sync storage error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("Sync storage ok");
    }

    @PostMapping("syncMetadataConfiguration")
    public ResponseEntity syncMetadataConfiguration(@RequestBody SyncMetadataDto syncMetadataDto) {
        try {
            if (SyncMetadataEnum.ADD_OR_UPDATE.getType().equals(syncMetadataDto.getSyncMetadataEnum().getType())) {
                configurationManagementService.addOrUpdateMetadataConfiguration(syncMetadataDto.getMutableMetadataConfiguration());
            } else if (SyncMetadataEnum.DELETE.getType().equals(syncMetadataDto.getSyncMetadataEnum().getType())) {
                configurationManagementService.deleteMetadataConfig(syncMetadataDto.getMutableMetadataConfiguration().getKey());
            }
            logger.info("Sync syncMetadataConfiguration success");
        } catch (Exception e) {
            logger.error("Sync syncMetadataConfiguration error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("Sync syncMetadataConfiguration ok");
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
                logger.info("Sync  save cron job success");
            } else if (syncCronJobDto.getSyncCornJobEnum().getType() == 2) {
                cronTaskConfigurationService.deleteConfiguration(syncCronJobDto.getConfigurationDto().getUuid());
                logger.info("Sync delete cron job success");
            }
        } catch (Exception e) {
            logger.error("Sync cronJob error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("Sync syncCronJob ok");
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
                logger.info("Sync add or update unionRepository success");
            }
        } catch (Exception e) {
            logger.error("Sync unionRepository error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("Sync unionRepository ok");
    }

    /**
     * 同步全局配置
     *
     * @param syncServerSettingsDto 全局配置
     * @return 返回结果
     */
    @PostMapping("syncServerSettings")
    public ResponseEntity syncServerSettings(@RequestBody SyncServerSettingsDto syncServerSettingsDto) {
        try {
            if (SyncServerSettingsEnum.ADD_OR_UPDATE.getType().equals(syncServerSettingsDto.getSyncServerSettingsEnum().getType())) {
                commonComponent.updateServerSettings(syncServerSettingsDto.getServerSettingsForm());
                logger.info("Sync update serverSettings success");
            }
        } catch (Exception e) {
            logger.error("Sync serverSettings error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("Sync serverSettings ok");
    }

    /**
     * 同步Ldap配置
     *
     * @param syncLdapDto Ldap配置
     * @return 返回结果
     */
    @PostMapping("syncLdap")
    public ResponseEntity syncLdap(@RequestBody SyncLdapDto syncLdapDto) {
        try {
            if (SyncLdapEnum.ADD_OR_UPDATE.getType().equals(syncLdapDto.getSyncLdapEnum().getType())) {
                commonComponent.updateLdap(syncLdapDto.getLdapConfiguration());
                logger.info("Sync update ldap success");
            }
        } catch (Exception e) {
            logger.error("Sync ldap error {}", ExceptionUtils.getStackTrace(e));
            return getBadRequestResponseEntity(e.getMessage(), "");
        }
        return ResponseEntity.ok("Sync ldap ok");
    }
}
