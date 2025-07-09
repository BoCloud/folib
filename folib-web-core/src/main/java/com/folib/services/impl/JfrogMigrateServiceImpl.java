/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.github.pagehelper.PageInfo;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.DistributedCacheComponent;
import com.folib.components.DistributedCounterComponent;
import com.folib.components.DistributedQueueComponent;
import com.folib.components.DistributedTopicComponent;
import com.folib.components.IdGenerateUtils;
import com.folib.components.syncartifact.SyncArtifactProvider;
import com.folib.components.syncartifact.SyncArtifactProviderRegistry;
import com.folib.configuration.ConfigurationUtils;
import com.folib.constant.GlobalConstants;
import com.folib.controllers.BaseController;
import com.folib.converters.migrate.JfrogMigrateConvert;
import com.folib.domain.SecurityRole;
import com.folib.domain.SecurityRoleEntity;
import com.folib.domain.adapter.jfrog.JfrogMapping;
import com.folib.domain.adapter.jfrog.JfrogMappingEnum;
import com.folib.domain.migrate.AddRepositoryForm;
import com.folib.domain.migrate.ArtifactMigrateInfo;
import com.folib.domain.migrate.SyncArtifactForm;
import com.folib.dto.AccessModelDTO;
import com.folib.dto.AccessResourcesDTO;
import com.folib.dto.AccessUserGroupsDTO;
import com.folib.dto.AccessUsersDTO;
import com.folib.dto.RoleDTO;
import com.folib.entity.Dict;
import com.folib.entity.FolibRole;
import com.folib.entity.MigrateInfo;
import com.folib.entity.UserGroup;
import com.folib.enums.ArtifactSyncTypeEnum;
import com.folib.enums.MigrateStatusEnum;
import com.folib.enums.NotifyScopesTypeEnum;
import com.folib.enums.StorageProviderEnum;
import com.folib.forms.JfrogMigrateForm;
import com.folib.forms.dict.DictForm;
import com.folib.mapper.UserGroupMapper;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.scanner.common.msg.TableResultResponse;
import com.folib.scanner.common.util.UUIDUtils;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.JfrogMigrateService;
import com.folib.services.MigrateInfoService;
import com.folib.services.RepositoryManagementService;
import com.folib.services.StorageManagementService;
import com.folib.storage.Storage;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.storage.repository.remote.RemoteRepositoryDto;
import com.folib.users.domain.SystemRole;
import com.folib.users.dto.UserDto;
import com.folib.users.service.FolibRoleService;
import com.folib.users.service.ResourceService;
import com.folib.users.service.UserGroupService;
import com.folib.users.service.UserService;
import com.folib.users.service.impl.EncodedPasswordUser;
import com.folib.users.service.impl.RelationalDatabaseUserService;
import com.folib.utils.SecurityUtils;
import com.folib.utils.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.jfrog.artifactory.client.Artifactory;
import org.jfrog.artifactory.client.ArtifactoryClientBuilder;
import org.jfrog.artifactory.client.Repositories;
import org.jfrog.artifactory.client.model.Group;
import org.jfrog.artifactory.client.model.LightweightRepository;
import org.jfrog.artifactory.client.model.PermissionTarget;
import org.jfrog.artifactory.client.model.Principal;
import org.jfrog.artifactory.client.model.Principals;
import org.jfrog.artifactory.client.model.Privilege;
import org.jfrog.artifactory.client.model.RemoteRepository;
import org.jfrog.artifactory.client.model.RepositorySummary;
import org.jfrog.artifactory.client.model.User;
import org.jfrog.artifactory.client.model.VirtualRepository;
import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl.LOCAL;
import static org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl.REMOTE;
import static org.jfrog.artifactory.client.model.impl.RepositoryTypeImpl.VIRTUAL;

/**
 * @author veadan
 * @since 2024-10-22 17:01
 */

@Slf4j
@Service
public class JfrogMigrateServiceImpl extends BaseController implements JfrogMigrateService {

    @Inject
    @RelationalDatabaseUserService.RelationalDatabase
    private UserService userService;

    @Resource
    private SecurityUtils securityUtils;

    @Resource
    private UserGroupService userGroupService;

    @Resource
    private UserGroupMapper userGroupMapper;

    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Resource
    private RepositoryManagementService repositoryManagementService;

    @Resource
    private StorageManagementService storageManagementService;

    @Resource
    private ConfigurationManagementService configurationManagementService;

    @Resource
    private LayoutProviderRegistry layoutProviderRegistry;

    @Resource
    private ResourceService resourceService;

    @Resource
    private FolibRoleService folibRoleService;

    @Resource
    private SyncArtifactProviderRegistry syncArtifactProviderRegistry;


    @Resource
    private DistributedCacheComponent distributedCacheComponent;

    @Resource
    private DistributedCounterComponent distributedCounterComponent;

    @Resource
    private MigrateInfoService migrateInfoService;

    private final static String DEFAULT_STORAGE = "jfrog-storage";

    private static final String USER = "USER";
    private static final String GROUP = "GROUP";
    private static final String PERMISSION = "PERMISSION";

    private static final String REPOSITORY = "REPOSITORY";
    private final static String JFROG_PREFIX = "/artifactory";

    private final static String DICT_TYPE = "artifact_migrate_task";
    private final static String QUEUE_NAME = "artifact_migrate_queue";


    private final static Map<String, List<Integer>> STATUS_MAPPING = new HashMap<>();


    private static ThreadPoolExecutor executor;

    // 0-初始 1-排队 2-获取索引 3-同步制品 4-暂停 5-完成
    static {
        STATUS_MAPPING.put("pending", List.of(MigrateStatusEnum.INITIAL.getStatus(), MigrateStatusEnum.INDEX_FAILED.getStatus(), MigrateStatusEnum.SYNCING_FAILED.getStatus()));
        STATUS_MAPPING.put("migrating", List.of(MigrateStatusEnum.PAUSED.getStatus(), MigrateStatusEnum.QUEUING.getStatus(), MigrateStatusEnum.FETCHING_INDEX.getStatus(), MigrateStatusEnum.SYNCING_ARTIFACT.getStatus()));
        STATUS_MAPPING.put("completed", List.of(MigrateStatusEnum.COMPLETED.getStatus()));
    }

    @Resource
    private DictServiceImpl dictService;


    @Resource
    private DistributedQueueComponent distributedQueueComponent;


    @Resource
    private DistributedTopicComponent distributedTopicComponent;

    @PostConstruct
    public void initThreadPool() {
        executor = new ThreadPoolExecutor(1, 1, 60L, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
    }


    @Async
    @Override
    public void migrate(JfrogMigrateForm form) {
        try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(form.getUrl() + JFROG_PREFIX).setUsername(form.getUsername()).setPassword(form.getPassword()).build()) {
            Map<String, Long> groupMap = null;
            // 先更新用户组
            if (form.getContents().contains(GROUP)) {
                groupMap = groupMigrate(artifactory);
            }
            // 同步用户及用户组关联关系
            if (form.getContents().contains(USER)) {
                userMigrate(artifactory, groupMap);
            }
            if (form.getContents().contains(REPOSITORY)) {
                // 创建存储空间
                String storageId = StringUtils.isBlank(form.getStorageId()) ? DEFAULT_STORAGE : form.getStorageId();
                form.setStorageId(storageId);
                // 判断存储空间是否存在，不存在新建
                Assert.isTrue(createStorageIfNotExist(form), "failed to create storage");
                // 同步仓库
                repositoryMigrate(storageId, artifactory, form);
                // 同步权限
                if (form.getContents().contains(GROUP) && form.getContents().contains(USER) && form.getContents().contains(REPOSITORY)) {
                    permissionMigrate(artifactory, storageId, groupMap);
                }
            }

        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RuntimeException(e.getMessage());
        }

    }

    @Override
    public List<Dict> getMigrateTask() {
        Dict dict = new Dict();
        dict.setDictType(DICT_TYPE);
        return dictService.selectDict(dict);
    }

    @Override
    public TableResultResponse<MigrateInfo> getRepositoryByMigrateId(int page, int limit, String migrateId, String status, String repoName) {
        List<Integer> statuses = STATUS_MAPPING.get(status);
        Assert.notNull(statuses, "无效的状态标识");
        PageInfo<MigrateInfo> pages = migrateInfoService.selectByMigrateIdAndStatus(migrateId, statuses, page, limit, repoName);
        return new TableResultResponse<>(pages.getTotal(), pages.getList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addSyncRepository(AddRepositoryForm form) {
        String migrateId = form.getMigrateId();
        ArtifactMigrateInfo info = getInfoByMigrate(migrateId);
        Assert.notNull(info, "无效的迁移任务");
        Map<String, String> reposUsed = new HashMap<>();
        Map<String, org.jfrog.artifactory.client.model.Repository> jfrogInfo = new HashMap<>();
        try (Artifactory artifactory = ArtifactoryClientBuilder.create().setUrl(info.getRemotePreUrl()).setUsername(info.getUsername()).setPassword(info.getPassword()).build()) {
            reposUsed = artifactory.storage().getStorageInfo().getRepositoriesSummaryList()
                    .stream().collect(Collectors.toMap(RepositorySummary::getRepoKey, RepositorySummary::getUsedSpace));
            for (String storeAndRepo : form.getStoreAndRepos()) {
                String repositoryId = ConfigurationUtils.getRepositoryId(storeAndRepo);
                try {
                    org.jfrog.artifactory.client.model.Repository repository = artifactory.repository(repositoryId).get();
                    jfrogInfo.put(repositoryId, repository);
                } catch (Exception e) {
                    throw new RuntimeException("jfrog中不存在仓库:" + repositoryId);
                }
            }
        }
        for (String storeAndRepo : form.getStoreAndRepos()) {
            String storageId = ConfigurationUtils.getStorageId(storeAndRepo, storeAndRepo);
            String repositoryId = ConfigurationUtils.getRepositoryId(storeAndRepo);
            StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
            if (storage == null) {
                continue;
            }
            RepositoryDto repository = storage.getRepository(repositoryId);
            if (repository == null) {
                continue;
            }
            repository.setType(RepositoryTypeEnum.PROXY.getType());
            RemoteRepositoryDto remoteDTO = new RemoteRepositoryDto();
            remoteDTO.setUsername(info.getUsername());
            remoteDTO.setPassword(info.getPassword());
            if (repository.getLayout().equals(DockerCoordinates.LAYOUT_NAME)) {
                remoteDTO.setUrl(info.getRemotePreUrl() + "/v2/" + repositoryId);
            } else {
                remoteDTO.setUrl(info.getRemotePreUrl() + "/" + repositoryId);
            }
            remoteDTO.setAutoBlocking(true);
            remoteDTO.setDownloadRemoteIndexes(true);
            remoteDTO.setChecksumValidation(true);
            remoteDTO.setAllowsDirectoryBrowsing(true);
            repository.setRemoteRepository(remoteDTO);
            try {
                configurationManagementService.saveRepository(storageId, repository);
            } catch (IOException e) {
                log.error("存储空间{},仓库{}修改失败", storageId, repositoryId);
                continue;
            }
            // 添加或更新
            MigrateInfo exist = migrateInfoService.getByMigrateIdAndRepoInfo(migrateId, storageId, repositoryId);
            if (exist == null) {
                org.jfrog.artifactory.client.model.Repository jfrog = jfrogInfo.get(repositoryId);
                MigrateInfo migrateInfo = new MigrateInfo();
                migrateInfo.setMigrateId(migrateId);
                migrateInfo.setLayout(jfrog.getRepositorySettings().getPackageType().name());
                migrateInfo.setUsedSpace(reposUsed.get(repositoryId));
                migrateInfo.setSyncStatus(0);
                migrateInfo.setIndexFinish(0);
                migrateInfo.setStorageId(storageId);
                migrateInfo.setRepositoryId(repositoryId);
                migrateInfo.setPostLayout(repository.getSubLayout());
                migrateInfo.setSyncProperty(info.getSyncMeta());
                migrateInfoService.save(migrateInfo);
            } else {
                exist.setSyncStatus(0);
                exist.setTotalArtifact(0);
                exist.setSuccessMount(0);
                exist.setIndexFinish(0);
                migrateInfoService.updateById(exist);
            }
        }
        int count = migrateInfoService.countByMigrateId(migrateId);
        info.setTotal(count);
        Dict dict = createDictByMigrate(info);
        DictForm dictForm = DictForm.builder().build();
        BeanUtils.copyProperties(dict, dictForm);
        dictService.updateDict(dictForm);
    }


    @Override
    public void startMigrate(String migrateId, List<String> storeAndRepos) {
        for (String storeAndRepo : storeAndRepos) {
            String storageId = ConfigurationUtils.getStorageId(storeAndRepo, storeAndRepo);
            String repositoryId = ConfigurationUtils.getRepositoryId(storeAndRepo);
            MigrateInfo info = migrateInfoService.getByMigrateIdAndRepoInfo(migrateId, storageId, repositoryId);
            if (info == null) {
                return;
            }
            if (MigrateStatusEnum.SYNCING_FAILED.getStatus() == info.getSyncStatus()) {
                info.setIndexFinish(1);
            }
            info.setSyncStatus(MigrateStatusEnum.QUEUING.getStatus());
            // 修改状态
            migrateInfoService.updateById(info);
            try {
                distributedQueueComponent.putToQueue(QUEUE_NAME, storeAndRepo);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            // 通知其他实例处理
            distributedTopicComponent.publishMessage(TOPIC_QUEUE, migrateId);
        }
    }

    @Override
    public void pauseMigrate(String migrateId, List<String> storeAndRepos) {
        for (String storeAndRepo : storeAndRepos) {
            String keyName = PAUSED_FLAG_PRE + storeAndRepo;
            distributedCacheComponent.put(keyName, "0");
        }
    }

    @Override
    public void setFailed(List<Long> ids) {
        for (Long id : ids) {
            MigrateInfo info = migrateInfoService.getById(id);
            if (info == null) {
                continue;
            }
            Integer syncStatus = info.getSyncStatus();
            if (MigrateStatusEnum.FETCHING_INDEX.getStatus() == syncStatus) {
                info.setSyncStatus(MigrateStatusEnum.INDEX_FAILED.getStatus());
                info.setIndexFinish(0);
            } else if (MigrateStatusEnum.SYNCING_ARTIFACT.getStatus() == syncStatus) {
                info.setSyncStatus(MigrateStatusEnum.SYNCING_FAILED.getStatus());
                info.setIndexFinish(1);
            } else if (MigrateStatusEnum.QUEUING.getStatus() == syncStatus) {
                info.setSyncStatus(MigrateStatusEnum.INITIAL.getStatus());
                info.setIndexFinish(0);
            }
            migrateInfoService.updateById(info);
        }
    }

    public void listenTask(String migrateId) {
        Dict dict = getDictByMigrateId(migrateId);
        if (Objects.isNull(dict)) {
            return;
        }
        ArtifactMigrateInfo info = JSON.parseObject(dict.getAlias(), ArtifactMigrateInfo.class);
        int batchSize = info.getBatchSize();
        // 修改最大线程数为配置内容
        if (batchSize > executor.getMaximumPoolSize()) {
            executor.setMaximumPoolSize(batchSize);
            executor.setCorePoolSize(batchSize);
        } else {
            executor.setCorePoolSize(batchSize);
        }

        executor.submit(() -> {
            taskHandler(migrateId, info);
        });
    }

    void taskHandler(String migrateId, ArtifactMigrateInfo info) {
        try {
            securityUtils.setAdminAuthentication();
            String pausedTask = PAUSED_QUEUE.poll();
            String storeAndRepo = pausedTask == null ? distributedQueueComponent.pollFromQueue(QUEUE_NAME, 10, TimeUnit.SECONDS) : pausedTask;
            while (storeAndRepo != null) {
                String storageId = ConfigurationUtils.getStorageId(storeAndRepo, storeAndRepo);
                String repositoryId = ConfigurationUtils.getRepositoryId(storeAndRepo);
                Storage storage = configurationManager.getConfiguration().getStorage(storageId);
                if (Objects.isNull(storage)) {
                    log.info("无效的存储空间{}", storageId);
                    continue;
                }
                Repository repository = storage.getRepository(repositoryId);
                if (Objects.isNull(repository)) {
                    log.info("无效的仓库{}", repositoryId);
                    continue;
                }
                log.info("开始迁移存储空间{}的仓库{}", storageId, repositoryId);
                SyncArtifactProvider syncArtifactProvider = syncArtifactProviderRegistry.getProvider(ArtifactSyncTypeEnum.resolveType(repository.getLayout()));
                if (syncArtifactProvider != null) {
                    SyncArtifactForm form = new SyncArtifactForm();
                    form.setDom("a");
                    form.setRepositoryId(repositoryId);
                    form.setMigrateId(migrateId);
                    form.setStorageId(storageId);
                    form.setBrowseUrl(StringUtils.removeEnd(info.getBrowsePrefix(), GlobalConstants.SEPARATOR) + GlobalConstants.SEPARATOR + repositoryId);
                    form.setMaxThreadNum(info.getThreadNumber());
                    form.setApiUrl(info.getRemotePreUrl());
                    form.setUsername(info.getUsername());
                    form.setPassword(info.getPassword());
                    form.setSyncMeta(info.getSyncMeta());
                    syncArtifactProvider.batchBrowseSync(form);
                } else {
                    MigrateInfo repoInfo = migrateInfoService.getByMigrateIdAndRepoInfo(migrateId, storageId, repositoryId);
                    repoInfo.setSyncStatus(MigrateStatusEnum.COMPLETED.getStatus());
                    migrateInfoService.updateById(repoInfo);
                }
                log.info("存储空间{}的仓库{}迁移结束", storageId, repositoryId);
                pausedTask = PAUSED_QUEUE.poll();
                storeAndRepo = pausedTask == null ? distributedQueueComponent.pollFromQueue(QUEUE_NAME, 10, TimeUnit.SECONDS) : pausedTask;
            }
        } catch (Exception e) {
            log.error("迁移出现异常{}", e.getMessage(), e);
        } finally {
            securityUtils.clearAuthentication();
        }
    }

    public Map<String, String> getFinishedCount(String migrateId, List<String> storeAndRepos) {
        HashMap<String, String> result = new HashMap<>();
        for (String storeAndRepo : storeAndRepos) {
            long artCount = distributedCounterComponent.getAtomicLong(JfrogMigrateService.ARTIFACT_COUNT + storeAndRepo).get();
            long artTotal = distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + storeAndRepo).get();
            long directoryTotal = distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_TOTAl + storeAndRepo).get();
            long directoryCount = distributedCounterComponent.getAtomicLong(JfrogMigrateService.DIRECTORY_COUNT + storeAndRepo).get();
            long up = artCount + directoryCount;
            long down = artTotal + directoryTotal;
            BigDecimal process = new BigDecimal(up).divide(new BigDecimal(down), 4, RoundingMode.HALF_UP).multiply(new BigDecimal(100));
            result.put(storeAndRepo, process.toString());
        }
        return result;
    }

    @Override
    public Map<String, Long> getIndexCount(String migrateId, List<String> storeAndRepos) {
        HashMap<String, Long> result = new HashMap<>();
        for (String storeAndRepo : storeAndRepos) {
            long count = distributedCounterComponent.getAtomicLong(JfrogMigrateService.INDEX_COUNT + storeAndRepo).get();
            result.put(storeAndRepo, count);
        }
        return result;
    }

    @Override
    public void repoContinue(String migrateId, List<String> storeAndRepos) {
        for (String storeAndRepo : storeAndRepos) {
            String storageId = ConfigurationUtils.getStorageId(storeAndRepo, storeAndRepo);
            String repositoryId = ConfigurationUtils.getRepositoryId(storeAndRepo);
            MigrateInfo repository = migrateInfoService.getByMigrateIdAndRepoInfo(migrateId, storageId, repositoryId);
            if (MigrateStatusEnum.INDEX_FAILED.getStatus() == repository.getSyncStatus()) {
                repository.setSyncStatus(MigrateStatusEnum.QUEUING.getStatus());
                // 制品失败和暂停都要去原实例
            } else if (MigrateStatusEnum.SYNCING_FAILED.getStatus() == repository.getSyncStatus()) {
                repository.setSyncStatus(MigrateStatusEnum.SYNCING_ARTIFACT.getStatus());
                distributedTopicComponent.publishMessage(TOPIC_PAUSED, repository.getStorageIdAndRepositoryId());
            } else if (MigrateStatusEnum.PAUSED.getStatus() == repository.getSyncStatus()) {
                repository.setSyncStatus(MigrateStatusEnum.SYNCING_ARTIFACT.getStatus());
                distributedTopicComponent.publishMessage(TOPIC_PAUSED, repository.getStorageIdAndRepositoryId());
            }
            try {
                // 通知各个节点
                distributedQueueComponent.putToQueue(QUEUE_NAME, storeAndRepo);
                distributedTopicComponent.publishMessage(TOPIC_QUEUE, repository.getMigrateId());
                migrateInfoService.save(repository);
            } catch (Exception e) {
                log.error("更新状态失败");
            }
        }

    }

    @Override
    public void repoFinish(String migrateId, List<String> storeAndRepos) {
        for (String storeAndRepo : storeAndRepos) {
            String storageId = ConfigurationUtils.getStorageId(storeAndRepo, storeAndRepo);
            String repositoryId = ConfigurationUtils.getRepositoryId(storeAndRepo);
            StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
            if (storage == null) {
                continue;
            }
            RepositoryDto repository = storage.getRepository(repositoryId);
            if (repository == null) {
                continue;
            }
            repository.setType(RepositoryTypeEnum.HOSTED.getType());
            repository.setAllowsDeployment(Boolean.TRUE);
            repository.setAllowsRedeployment(Boolean.TRUE);
            try {
                configurationManagementService.saveRepository(storageId, repository);
            } catch (Exception e) {
                log.error("更新状态失败");
            }
            // 同步更新
            MigrateInfo info = migrateInfoService.getByMigrateIdAndRepoInfo(migrateId, storageId, repositoryId);
            info.setSyncStatus(MigrateStatusEnum.END.getStatus());
            migrateInfoService.updateById(info);
        }
    }

    @Override
    public void changeLayout(MigrateInfo info) {
        Assert.notNull(info.getPostLayout(), "修改后的布局不能为空");
        JfrogMappingEnum subLayout = JfrogMappingEnum.getEnumBySubLayout(info.getPostLayout());
        Assert.notNull(subLayout, "无效的新布局");
        StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(info.getStorageId());
        Assert.notNull(storage, "未找到对应的存储空间");
        RepositoryDto repository = storage.getRepository(info.getRepositoryId());
        Assert.notNull(repository, "未找到对应的仓库");
        repository.setLayout(subLayout.getLayout());
        repository.setSubLayout(subLayout.getSubLayout());
        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(subLayout.getLayout());
        if (Objects.nonNull(layoutProvider)) {
            repository.setArtifactCoordinateValidators(layoutProvider.getDefaultArtifactCoordinateValidators());
        }
        try {

            configurationManagementService.saveRepository(info.getStorageId(), repository);
        } catch (Exception e) {
            log.error("更新状态失败");
        }
        migrateInfoService.updateById(info);
    }

    @Override
    public void deleteTask(Long id) {
        Dict dict = dictService.getById(id);
        // 更具dict 获取所有仓库 判断是否有进行中的
        ArtifactMigrateInfo info = JSON.parseObject(dict.getAlias(), ArtifactMigrateInfo.class);
        String migrateId = info.getMigrateId();
        List<Integer> status = STATUS_MAPPING.get("migrating");
        List<MigrateInfo> doing = migrateInfoService.selectByMigrateId(migrateId, status);
        Assert.isTrue(doing.isEmpty(), "有正在迁移的仓库，不能删除任务");
        dictService.deleteDictById(id);
        migrateInfoService.deleteByMigrateId(migrateId);

    }

    @Override
    public void restartRepo(String migrateId) {
        // 将所有的仓库都变成待迁移状态
        List<Integer> status = List.of(MigrateStatusEnum.COMPLETED.getStatus(), MigrateStatusEnum.END.getStatus());
        List<MigrateInfo> done = migrateInfoService.selectByMigrateId(migrateId, status);
        Dict dict = getDictByMigrateId(migrateId);
        ArtifactMigrateInfo info = JSON.parseObject(dict.getAlias(), ArtifactMigrateInfo.class);
        for (MigrateInfo migrateInfo : done) {
            if (MigrateStatusEnum.END.getStatus() == migrateInfo.getSyncStatus()) {
                String storageId = migrateInfo.getStorageId();
                String repositoryId = migrateInfo.getRepositoryId();
                StorageDto storage = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
                if (storage == null) {
                    continue;
                }
                RepositoryDto repository = storage.getRepository(repositoryId);
                if (repository == null) {
                    continue;
                }
                repository.setType(RepositoryTypeEnum.PROXY.getType());
                RemoteRepositoryDto remoteDTO = new RemoteRepositoryDto();
                remoteDTO.setUsername(info.getUsername());
                remoteDTO.setPassword(info.getPassword());
                if (repository.getLayout().equals(DockerCoordinates.LAYOUT_NAME)) {
                    remoteDTO.setUrl(info.getRemotePreUrl() + "/v2/" + repositoryId);
                } else {
                    remoteDTO.setUrl(info.getRemotePreUrl() + "/" + repositoryId);
                }
                remoteDTO.setAutoBlocking(true);
                remoteDTO.setDownloadRemoteIndexes(true);
                remoteDTO.setChecksumValidation(true);
                remoteDTO.setAllowsDirectoryBrowsing(true);
                repository.setRemoteRepository(remoteDTO);
                try {
                    configurationManagementService.saveRepository(storageId, repository);
                } catch (IOException e) {
                    log.error("存储空间{},仓库{}修改失败", storageId, repositoryId);
                }
            }
            migrateInfo.setSyncStatus(MigrateStatusEnum.INITIAL.getStatus());
            migrateInfo.setIndexFinish(0);
            migrateInfo.setTotalArtifact(0);
            migrateInfo.setSuccessMount(0);
            migrateInfoService.updateById(migrateInfo);
        }

    }

    @Override
    public List<String> getAllRepo(String migrateId) {
        List<MigrateInfo> migrateInfos = migrateInfoService.selectByMigrateId(migrateId, null);
        List<String> repos = new ArrayList<>();
        for (MigrateInfo migrateInfo : migrateInfos) {
            repos.add(migrateInfo.getRepositoryId());
        }
        return repos;
    }


    private Dict getDictByMigrateId(String migrateId) {
        Dict dict = new Dict();
        dict.setDictType(DICT_TYPE);
        dict.setDictKey(migrateId);
        List<Dict> dicts = dictService.selectDict(dict);
        if (dicts.size() != 1) {
            return null;
        } else {
            return dicts.get(0);
        }
    }

    @Override
    public void addTask(ArtifactMigrateInfo info) {
        Dict dict = new Dict();
        dict.setCreateTime(new Date());
        dict.setDictType(DICT_TYPE);
        String migrateId = UUIDUtils.generateUuid();
        dict.setDictKey(migrateId);
        info.setMigrateId(migrateId);
        dict.setDictValue(UserUtils.getUsername());
        dict.setAlias(JSON.toJSONString(info));
        dictService.saveDict(dict);
    }

    @Override
    public void updateTask(Long id, ArtifactMigrateInfo info) {
        Dict dict = dictService.getById(id);
        Assert.notNull(dict, "无效的迁移任务");
        dict.setAlias(JSON.toJSONString(info));
        dictService.updateById(dict);
    }

    private Map<String, Long> groupMigrate(Artifactory artifactory) {
        try {
            log.info("begin to sync group ");
            Map<String, Long> groupMap = new HashMap<>();
            List<String> groupNames = artifactory.security().groupNames();
            List<UserGroup> groups = new LinkedList<>();
            List<UserGroup> adminGroups = new LinkedList<>();
            for (String groupName : groupNames) {
                Group group = artifactory.security().group(groupName);
                UserGroup userGroup = JfrogMigrateConvert.INSTANCE.jfrogGroupToFolib(group);
                saveIfNotExist(userGroup);
                groups.add(userGroup);
                if (group.isAdminPrivileges()) {
                    adminGroups.add(userGroup);
                }
            }
            groups.forEach(group -> {
                groupMap.put(group.getGroupName(), group.getId());
            });
            // 将是admin的用户组加入admin role里
            if (!adminGroups.isEmpty()) {
                // 获取admin角色的信息
                FolibRole folibRole = folibRoleService.queryById(SystemRole.ADMIN.name());
                RoleDTO roleDetail = folibRoleService.getRoleDetail(SystemRole.ADMIN.name(), folibRole);
                List<AccessUserGroupsDTO> roleGroups = roleDetail.getPrivileges().getGroups();
                for (UserGroup adminGroup : adminGroups) {
                    AccessUserGroupsDTO groupsDTO = new AccessUserGroupsDTO();
                    groupsDTO.setId(String.valueOf(adminGroup.getId()));
                    groupsDTO.setName(adminGroup.getGroupName());
                    roleGroups.add(groupsDTO);
                }
                List<AccessUserGroupsDTO> distinctGroups = new ArrayList<>(roleGroups.stream().collect(Collectors.toMap(AccessUserGroupsDTO::getId, g -> g, (existing, replacement) -> existing)).values());
                roleDetail.getPrivileges().setGroups(distinctGroups);
                folibRoleService.save(roleDetail, UserUtils.getUsername());
            }
            log.info("group info sync edn");
            return groupMap;
        } catch (Exception e) {
            log.info("failed to sync group {}", e.getMessage(), e);
            throw new RuntimeException(e.getMessage());
        }
    }

    private void userMigrate(Artifactory artifactory, Map<String, Long> groupMap) {
        Collection<String> userNames = artifactory.security().userNames();
        for (String userName : userNames) {
            User user = artifactory.security().user(userName);
            UserDto newUser = new UserDto();
            if (user.getRealm().equals("ldap")) {
                newUser.setSourceId("ldapUserDetailsService");
            } else {
                // 设置默认密码等于用户名
                newUser.setPassword("DayeKJjeRQ$4N3z");
            }
            newUser.setUsername(userName);
            newUser.setEmail(user.getEmail());
            newUser.setId(userName);
            // 必须为用户赋予一个角色
            if (user.isAdmin()) {
                SecurityRole securityRole = new SecurityRoleEntity(SystemRole.ADMIN.name());
                newUser.setRoles(Collections.singleton(securityRole));
            } else {
                SecurityRole securityRole = new SecurityRoleEntity(SystemRole.GENERAL.name());
                newUser.setRoles(Collections.singleton(securityRole));
            }
            // 判断用户是否存在 存在不操作
            com.folib.domain.User folibUser = userService.findByUsername(userName);
            if (folibUser != null) {
                continue;
            }
            // 同步用户组信息 如果groupMap为null代表没有同步用户组
            if (groupMap != null) {
                Collection<String> groups = user.getGroups();
                if (groups != null && !groups.isEmpty()) {
                    for (String group : groups) {
                        Long groupId = groupMap.get(group);
                        newUser.getUserGroupIds().add(String.valueOf(groupId));
                    }
                }
            }

            userService.save(new EncodedPasswordUser(newUser, passwordEncoder));
        }
    }

    // 增加添加迁移任务
    private void repositoryMigrate(String storageId, Artifactory artifactory, JfrogMigrateForm form) {
        Repositories repositories = artifactory.repositories();
        Storage storage = configurationManagementService.getConfiguration().getStorage(storageId);
        // 生成迁移信息
        ArtifactMigrateInfo migrateInfo = getMigrateInfo(form, storageId);
        List<LightweightRepository> repoList = new LinkedList<>();
        repoList.addAll(repositories.list(LOCAL));
        repoList.addAll(repositories.list(REMOTE));
        repoList.addAll(repositories.list(VIRTUAL));
        Map<String, String> reposUsed = artifactory.storage().getStorageInfo().getRepositoriesSummaryList()
                .stream().collect(Collectors.toMap(RepositorySummary::getRepoKey, RepositorySummary::getUsedSpace));
        for (LightweightRepository repository : repoList) {
            String repositoryId = repository.getKey();
            RepositoryDto repositoryDto = JfrogMapping.initRepoByPackageType(repository.getPackageType());
            if (repositoryDto == null) {
                log.error("don't  support the repository {} ", repository.getPackageType());
                continue;
            }
            repositoryDto.setId(repositoryId);
            if ("s3".equals(storage.getStorageProvider())) {
                String basedir = storage.getBasedir() + "/" + repositoryId;
                repositoryDto.setBasedir(basedir);
            }
            repositoryDto.setStorageProvider(storage.getStorageProvider());
            repositoryDto.setAllowsDeletion(true);
            repositoryDto.setAllowsDeployment(true);
            repositoryDto.setAllowsDirectoryBrowsing(true);
            setRepositoryInfo(repository, repositoryDto, artifactory, storageId, form, migrateInfo, reposUsed);
            groupRepositoryValid(storageId, repositoryDto);
            RepositoryDto newRepo;
            try {
                configurationManagementService.saveRepository(storageId, repositoryDto);
                newRepo = getMutableConfigurationClone().getStorages().get(storageId).getRepository(repositoryId);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(newRepo));
                if (!Files.exists(repositoryPath)) {
                    repositoryManagementService.createRepository(storageId, repositoryId);
                }
            } catch (Exception ex) {
                logger.error("Failed to create the repository path {}!", repositoryId, ex);
                try {
                    configurationManagementService.removeRepository(storageId, repositoryId);
                } catch (Exception e) {
                    logger.error("Failed to remove the repository {}!", repositoryId, e);
                }
                throw new RuntimeException(ex.getMessage());
            }
            if (!RepositoryTypeEnum.GROUP.getType().equals(repositoryDto.getType())) {
                //初始化仓库数据
                @SuppressWarnings("all")
                LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryDto.getLayout());
                layoutProvider.initData(storageId, repositoryId);
            }
            String resourceId = storageId + "_" + repositoryId;
            com.folib.entity.Resource resource = resourceService.queryById(resourceId);
            if (Objects.equals(null, resource)) {
                resourceService.insert(com.folib.entity.Resource.builder()
                        .id(resourceId.toUpperCase())
                        .storageId(storageId)
                        .repositoryId(repositoryId)
                        .build());
            }
        }
        // 查看是否有对应的迁移id
        int cnt = migrateInfoService.countByMigrateId(migrateInfo.getMigrateId());
        if (cnt > 0) {
            migrateInfo.setTotal(cnt);
            migrateInfo.setSyncMeta(1);
            Dict dict = createDictByMigrate(migrateInfo);
            dictService.saveOrUpdateByTypeAndKey(dict);
        }
    }

    void setRepositoryInfo(LightweightRepository repository, RepositoryDto repositoryDto, Artifactory
            artifactory, String storageId, JfrogMigrateForm form, ArtifactMigrateInfo migrateInfo, Map<String, String> reposUsed) {
        if (repository.getType() == LOCAL) {
            if ("2".equals(form.getArtifactType())) {
                repositoryDto.setType(RepositoryTypeEnum.PROXY.getType());
                // 获取同步仓库的
                RemoteRepositoryDto remoteDTO = new RemoteRepositoryDto();
                remoteDTO.setUsername(form.getUsername());
                remoteDTO.setPassword(form.getPassword());
                remoteDTO.setAutoBlocking(true);
                remoteDTO.setDownloadRemoteIndexes(true);
                remoteDTO.setChecksumValidation(true);
                remoteDTO.setAllowsDirectoryBrowsing(true);
                // 如果是docker
                if ("Docker".equals(repository.getPackageType())) {
                    String remoteUrl = repository.getUrl();
                    String replace = remoteUrl.replace(repository.getKey(), "v2/" + repository.getKey());
                    remoteDTO.setUrl(replace);
                } else {
                    remoteDTO.setUrl(repository.getUrl());
                }
                repositoryDto.setRemoteRepository(remoteDTO);
                createAndSaveMigrateInfo(migrateInfo, repository, storageId, reposUsed);
            } else {
                repositoryDto.setType(RepositoryTypeEnum.HOSTED.getType());
            }

        } else if (repository.getType() == REMOTE) {
            // 代理库要获取远程地址
            repositoryDto.setType(RepositoryTypeEnum.PROXY.getType());
            RemoteRepositoryDto remoteDTO = new RemoteRepositoryDto();
            RemoteRepository remoteRepository = (RemoteRepository) artifactory.repository(repository.getKey()).get();
            remoteDTO.setUrl(repository.getUrl());
            remoteDTO.setUsername(remoteRepository.getUsername());
            remoteDTO.setPassword(remoteRepository.getPassword());
            repositoryDto.setRemoteRepository(remoteDTO);
        } else if (repository.getType() == VIRTUAL) {
            // 组合库要获取子仓库信息
            repositoryDto.setType(RepositoryTypeEnum.GROUP.getType());
            VirtualRepository virtualRepo = (VirtualRepository) artifactory.repository(repository.getKey()).get();
            Collection<String> repositories = virtualRepo.getRepositories();
            Set<String> groupRepository = new HashSet<>();
            for (String repo : repositories) {
                groupRepository.add(storageId + ":" + repo);
            }
            repositoryDto.setGroupRepositories(groupRepository);
        }
        MigrateInfo exist = migrateInfoService.getByMigrateIdAndRepoInfo(migrateInfo.getMigrateId(), storageId, repository.getKey());
        if (Objects.nonNull(exist) && StringUtils.isNotBlank(exist.getPostLayout())) {
            JfrogMappingEnum layout = JfrogMappingEnum.getEnumBySubLayout(exist.getPostLayout());
            if (Objects.nonNull(layout)) {
                repositoryDto.setLayout(layout.getLayout());
                repositoryDto.setSubLayout(layout.getSubLayout());
            }
        }

    }

    private MigrateInfo createAndSaveMigrateInfo(ArtifactMigrateInfo info, LightweightRepository repository, String storageId, Map<String, String> spaceInfo) {
        //新建同步数据入库
        MigrateInfo exist = migrateInfoService.getByMigrateIdAndRepoInfo(info.getMigrateId(), storageId, repository.getKey());
        if (Objects.isNull(exist)) {
            MigrateInfo migrateInfo = new MigrateInfo();
            migrateInfo.setMigrateType("jfrog");
            migrateInfo.setMigrateId(info.getMigrateId());
            migrateInfo.setStorageId(storageId);
            migrateInfo.setSyncProperty(1);
            migrateInfo.setIndexFinish(0);
            migrateInfo.setRepositoryId(repository.getKey());
            migrateInfo.setUsedSpace(spaceInfo.get(repository.getKey()));
            migrateInfo.setSyncStatus(0);
            migrateInfo.setLayout(repository.getPackageType());
            JfrogMappingEnum jfrogName = JfrogMappingEnum.getEnumByJfrogName(repository.getPackageType());
            if (jfrogName != null) {
                migrateInfo.setPostLayout(jfrogName.getSubLayout());
            }
            migrateInfoService.save(migrateInfo);
            return migrateInfo;
        } else {
            exist.setUsedSpace(spaceInfo.get(repository.getKey()));
            exist.setSyncStatus(MigrateStatusEnum.INITIAL.getStatus());
            exist.setIndexFinish(0);
            exist.setTotalArtifact(0);
            exist.setSuccessMount(0);
            migrateInfoService.updateById(exist);
            return exist;
        }
    }

    private ArtifactMigrateInfo getMigrateInfo(JfrogMigrateForm form, String storageId) {
        ArtifactMigrateInfo info = new ArtifactMigrateInfo();
        // 生成迁移id
        info.setMigrateId("jfrog-migrate:" + storageId);
        info.setStatus(0);
        info.setBatchSize(1);
        info.setThreadNumber(4);
        String url = StringUtils.removeEnd(form.getUrl(), GlobalConstants.SEPARATOR) + GlobalConstants.SEPARATOR + "artifactory";
        info.setBrowsePrefix(url);
        info.setRemotePreUrl(url);
        info.setUsername(form.getUsername());
        info.setPassword(form.getPassword());
        return info;
    }

    private Dict createDictByMigrate(ArtifactMigrateInfo info) {
        Dict dict = new Dict();
        dict.setDictType(DICT_TYPE);
        dict.setCreateTime(new Date());
        dict.setDictKey(info.getMigrateId());
        dict.setDictValue(UserUtils.getUsername());
        dict.setAlias(JSONObject.toJSONString(info));
        return dict;
    }


    private ArtifactMigrateInfo getInfoByMigrate(String migrateId) {
        Dict query = new Dict();
        query.setDictType(DICT_TYPE);
        query.setDictKey(migrateId);
        List<Dict> dicts = dictService.selectDict(query);
        if (dicts.size() != 1) {
            return null;
        }
        String dict = dicts.get(0).getAlias();
        return JSON.parseObject(dict, ArtifactMigrateInfo.class);
    }


    private void permissionMigrate(Artifactory artifactory, String storageId, Map<String, Long> groupMap) {
        List<String> permissionTargetNames = artifactory.security().permissionTargets();
        for (String permissionTargetName : permissionTargetNames) {
            PermissionTarget permission = artifactory.security().permissionTarget(permissionTargetName);
            log.info("jfrog permission is" + JSONObject.toJSONString(permission));
            Principals principals = permission.getPrincipals();
            AccessModelDTO folibPrincipals = new AccessModelDTO();
            List<AccessUserGroupsDTO> folibGroups = new LinkedList<>();
            List<AccessUsersDTO> folibUsers = new LinkedList<>();
            List<AccessResourcesDTO> folibResources = new LinkedList<>();
            // 用户组转换
            for (Principal group : principals.getGroups()) {
                AccessUserGroupsDTO folibGroup = new AccessUserGroupsDTO();
                folibGroup.setName(group.getName());
                folibGroup.setId(String.valueOf(groupMap.get(group.getName())));
                for (Privilege privilege : group.getPrivileges()) {
                    String access = JfrogMapping.accessConvert(privilege);
                    if (StringUtils.isNotBlank(access)) {
                        folibGroup.getAccess().add(access);
                    }
                }
                folibGroups.add(folibGroup);
            }
            // 用户转换 todo anonymous 用户的问题
            for (Principal user : principals.getUsers()) {
                AccessUsersDTO folibUser = new AccessUsersDTO();
                folibUser.setId(user.getName());
                for (Privilege privilege : user.getPrivileges()) {
                    String access = JfrogMapping.accessConvert(privilege);
                    if (StringUtils.isNotBlank(access)) {
                        folibUser.getAccess().add(access);
                    }
                }
                folibUsers.add(folibUser);
            }
            // 资源转换
            Set<String> paths = Arrays.stream(permission.getIncludesPattern().split(",")).collect(Collectors.toSet());
            for (String repository : permission.getRepositories()) {
                if (paths.contains("**")) {
                    AccessResourcesDTO folibResource = new AccessResourcesDTO();
                    folibResource.setStorageId(storageId);
                    folibResource.setRepositoryId(repository);
                    folibResources.add(folibResource);
                } else {
                    for (String path : paths) {
                        AccessResourcesDTO folibResource = new AccessResourcesDTO();
                        folibResource.setStorageId(storageId);
                        folibResource.setRepositoryId(repository);
                        folibResource.setPath(path);
                        folibResources.add(folibResource);
                    }
                }
            }
            // 整合权限对象
            RoleDTO roleDTO = new RoleDTO();
            roleDTO.setName(permission.getName());
            roleDTO.setDescription("Jfrog同步权限:" + permission.getName());
            folibPrincipals.setGroups(folibGroups);
            folibPrincipals.setUsers(folibUsers);
            roleDTO.setPrivileges(folibPrincipals);
            roleDTO.setResources(folibResources);
            folibRoleService.save(roleDTO, UserUtils.getUsername());
        }
    }

    public void saveIfNotExist(UserGroup userGroup) {
        String groupName = userGroup.getGroupName();
        List<UserGroup> userGroups = userGroupService.queryByGroupNames(Collections.singletonList(groupName));
        if (CollectionUtils.isNotEmpty(userGroups) && userGroups.get(0).getGroupName().equals(groupName)) {
            log.info("UserGroupName {} is already exist update groupName", groupName);
            userGroup.setIsDefault(null);
            userGroup.setDeleted(null);
            userGroup.setId(userGroups.get(0).getId());
        } else {
            userGroup.setId(idGenerateUtils.generateId("userGroupId"));
            userGroupMapper.insert(userGroup);
        }
    }

    public boolean createStorageIfNotExist(JfrogMigrateForm form) {
        // 判断存储空间是否存在
        String storageId = form.getStorageId();
        Storage existStorage = configurationManagementService.getConfiguration().getStorage(storageId);
        if (existStorage == null) {
            StorageDto storage = new StorageDto();
            storage.setId(storageId);
            storage.setAdmin(NotifyScopesTypeEnum.ADMIN.getScope());
            storage.setBasedir(form.getBasedir());
            if (StringUtils.isBlank(form.getStorageProvider())) {
                storage.setStorageProvider(StorageProviderEnum.LOCAL.getType());
            }
            try {
                storageManagementService.createStorage(storage);
            } catch (Exception e) {
                log.error("create storage failed{}", e.getMessage(), e);
                return false;
            }
        }
        return true;
    }

    private void groupRepositoryValid(String storageId, Repository repository) {
        if (Objects.isNull(repository) || CollectionUtils.isEmpty(repository.getGroupRepositories())) {
            return;
        }
        String storageIdAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repository.getId());
        if (repository.getGroupRepositories().contains(storageIdAndRepositoryId)) {
            throw new IllegalArgumentException("The combination repository cannot contain itself");
        }
    }

    @PreDestroy
    public void shutdownExecutor() {
        executor.shutdown();
        try {
            if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
                executor.shutdownNow();
            }
        } catch (InterruptedException e) {
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    // 从迁移任务中获取存在默认的jfrog配置信息，如果存在多条或不存在取最后一条
    public Dict getWebhookSetting() {
        Dict query = new Dict();
        query.setDictType("artifact_migrate_task");
        List<Dict> dicts = dictService.selectDict(query);
        if (CollectionUtils.isEmpty(dicts)) {
            return null;
        }
        ;
        for (Dict dict : dicts) {
            ArtifactMigrateInfo info = JSON.parseObject(dict.getAlias(), ArtifactMigrateInfo.class);
            if (info.getWebhookSetting() != null && info.getWebhookSetting() == 1) {
                return dict;
            }
        }
        return dicts.get(0);
    }

}
