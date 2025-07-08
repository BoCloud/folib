package com.folib.eventlistener.index;


import com.google.common.collect.Lists;
import com.folib.configuration.ConfigurationManager;
import com.folib.configuration.ConfigurationUtils;
import com.folib.event.AsyncEventListener;
import com.folib.event.index.IndexEvent;
import com.folib.event.index.IndexTypeEnum;
import com.folib.metadata.indexer.RpmGroupRepoIndexer;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.*;

@Slf4j
@Component
public class IndexEventListener {


    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactManagementService artifactManagementService;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationService;

    @Value("${folib.temp}")
    private String tempPath;

    @AsyncEventListener
    public void handle(final IndexEvent event) {
        log.info("IndexEventListener handle storageId: {}, repositoryId: {} ,type: {}", event.getStorageId(), event.getRepositoryId(),event.getIndexType().toString());
        if (event.getIndexType().equals(IndexTypeEnum.RPM)) {
            handleRpmGroupRepoIndexer(event);
        }
    }

    public void handleRpmGroupRepoIndexer(IndexEvent event) {
        Repository repository = configurationManager.getRepository(event.getStorageId(), event.getRepositoryId());
        if (repository == null) {
            log.error("Repository not found for storageId: {}, repositoryId: {}", event.getStorageId(), event.getRepositoryId());
            return;
        }

        List<String> storageAndRepositoryIdList = Lists.newArrayList();
        resolveGroupRepository(repository, storageAndRepositoryIdList);

        if (storageAndRepositoryIdList.isEmpty()) {
            log.warn("No storage and repository IDs found for repository: {}", repository.getId());
            return;
        }
        // 创建 RpmGroupRepoIndexer 实例，避免在循环中重复创建
        RpmGroupRepoIndexer rpmGroupRepoIndexer = new RpmGroupRepoIndexer(tempPath, repositoryPathResolver, artifactManagementService, configurationManager);

        for (String storageAndRepositoryId : storageAndRepositoryIdList) {
            try {
                Repository groupRepository = configurationManager.getRepository(storageAndRepositoryId);
                if (groupRepository == null) {
                    log.error("Group repository not found for storageAndRepositoryId: {}", storageAndRepositoryId);
                    continue;
                }
                if (!groupRepository.getType().equals("group")) {
                    log.warn("Group repository type is not group for storageAndRepositoryId: {}", storageAndRepositoryId);
                    continue;
                }
                rpmGroupRepoIndexer.aggregationIndexer(groupRepository, repository);
            } catch (Exception e) {
                log.error("Error handling RPM group repo indexer for storageAndRepositoryId: {}. Error: {}", storageAndRepositoryId, e.getMessage(), e);
            }
        }
    }

    /**
     * 解析并获取组仓库相关的存储和仓库ID列表
     *
     * 本方法旨在遍历所有存储和仓库，找出与指定组仓库相关的所有仓库ID和存储ID组合
     * 它首先确保输入列表是线程安全的，然后检查仓库及其关联的存储是否存在，
     * 最后遍历所有存储和仓库，通过调用辅助方法判断是否属于指定的组仓库
     *
     * @param repository 当前仓库对象，用于获取组仓库的根ID
     * @param storageAndRepositoryIdList 存储和仓库ID的列表，线程安全的
     * @return 返回包含所有与组仓库相关的存储和仓库ID的列表
     */
    public List<String> resolveGroupRepository(Repository repository, List<String> storageAndRepositoryIdList) {
        // 确保传入的列表是线程安全的
        List<String> threadSafeList = Collections.synchronizedList(storageAndRepositoryIdList);

        // 检查仓库及其存储是否为空，如果为空则直接返回线程安全列表
        if (repository == null || repository.getStorage() == null) {
            return threadSafeList;
        }

        // 构造根仓库的存储和仓库ID组合
        String rootStorageAndRepositoryId = ConfigurationUtils.getStorageIdAndRepositoryId(
                repository.getStorage().getId(),
                repository.getId()
        );

        // 获取所有存储的配置
        Map<String, Storage> storages = configurationService.getConfiguration().getStorages();

        // 如果没有存储配置或者配置为空，则直接返回线程安全列表
        if (storages == null || storages.isEmpty()) {
            return threadSafeList;
        }

        // 遍历所有存储
        for (String storageId : storages.keySet()) {
            Storage storage = storages.get(storageId);
            // 如果存储对象为空，则跳过当前循环
            if (storage == null) {
                continue;
            }

            // 遍历当前存储下的所有仓库
            for (String repositoryId : storage.getRepositories().keySet()) {
                Repository subRepository = storage.getRepository(repositoryId);
                // 判断子仓库是否属于指定的组仓库，如果是则添加到线程安全列表中
                if (isGroupRepositoryWithRootId(subRepository, rootStorageAndRepositoryId)) {
                    threadSafeList.add(ConfigurationUtils.getStorageIdAndRepositoryId(storageId, repositoryId));
                }
            }
        }

        // 返回包含所有相关存储和仓库ID的线程安全列表
        return threadSafeList;
    }

    /**
     * 判断仓库是否属于指定的组仓库
     * 本方法用于判断一个仓库是否是组仓库，并且包含指定的根仓库ID
     * 它首先检查仓库对象是否为空，然后检查仓库类型是否为组类型，
     * 最后检查仓库的组仓库集合中是否包含指定的根仓库ID
     *
     * @param subRepository 子仓库对象，需要判断的仓库
     * @param rootStorageAndRepositoryId 根仓库的存储和仓库ID组合，用于判断是否属于指定的组仓库
     * @return 如果仓库属于指定的组仓库则返回true，否则返回false
     */
    private boolean isGroupRepositoryWithRootId(Repository subRepository, String rootStorageAndRepositoryId) {
        // 检查子仓库对象是否为空
        if (subRepository == null) {
            return false;
        }
        // 检查仓库类型是否为组类型
        if (!RepositoryTypeEnum.GROUP.getType().equals(subRepository.getType())) {
            return false;
        }
        // 获取仓库的组仓库集合，并判断是否包含指定的根仓库ID
        Set<String> groupRepositories = Optional.ofNullable(subRepository.getGroupRepositories()).orElse(Collections.emptySet());
        return groupRepositories.contains(rootStorageAndRepositoryId);
    }
}
