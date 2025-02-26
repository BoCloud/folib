package com.veadan.folib.eventlistener.index;


import com.google.common.collect.Lists;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.event.index.IndexEvent;
import com.veadan.folib.event.index.IndexTypeEnum;
import com.veadan.folib.event.privilege.PrivilegeEvent;
import com.veadan.folib.metadata.indexer.RpmGroupRepoIndexer;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.List;

@Slf4j
@Component
public class IndexEventListener {


    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    @Inject
    protected ArtifactManagementService artifactManagementService;

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
        configurationManager.resolveGroupRepository(repository, storageAndRepositoryIdList);

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
}
