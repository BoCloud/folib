package com.veadan.folib.configuration;

import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.net.URI;
import java.util.List;
import java.util.Objects;

/**
 * @author mtodorov
 */
@Slf4j
@Component
public class ConfigurationManager implements StoragesConfigurationManager {

    @Inject
    @Lazy
    private ConfigurationManagementService configurationService;

    public Repository getRepository(String storageAndRepositoryId) {
        String[] elements = storageAndRepositoryId.split(":");
        String storageId = elements[0];
        String repositoryId = elements[1];

        return getConfiguration().getStorage(storageId).getRepository(repositoryId);
    }

    public Repository getRepository(String storageId,
                                    String repositoryId) {
        return getConfiguration().getStorage(storageId).getRepository(repositoryId);
    }

    public Storage getStorage(String storageId) {
        return getConfiguration().getStorage(storageId);
    }

    public Configuration getConfiguration() {
        return configurationService.getConfiguration();
    }

    public URI getBaseUri() {
        try {
            return URI.create(getConfiguration().getBaseUrl());
        } catch (IllegalArgumentException e) {
            throw new InvalidConfigurationException(e);
        }
    }

    public Integer getSessionTimeoutSeconds() {
        return getConfiguration().getSessionConfiguration().getTimeoutSeconds();
    }

    public List<String> resolveGroupRepository(Repository repository, List<String> storageAndRepositoryIdList) {
        if (CollectionUtils.isNotEmpty(repository.getGroupRepositories())) {
            for (String storageAndRepositoryId : repository.getGroupRepositories()) {
                String sId = ConfigurationUtils.getStorageId(storageAndRepositoryId, storageAndRepositoryId);
                String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
                try {
                    Storage storage = getConfiguration().getStorage(sId);
                    if (Objects.nonNull(storage)) {
                        Repository subRepository = storage.getRepository(rId);
                        if (Objects.nonNull(subRepository)) {
                            if (!isRepositoryResolvable(subRepository)) {
                                continue;
                            }
                            if (RepositoryTypeEnum.GROUP.getType().equals(subRepository.getType())) {
                                resolveGroupRepository(subRepository, storageAndRepositoryIdList);
                            } else if (!storageAndRepositoryIdList.contains(storageAndRepositoryId)) {
                                storageAndRepositoryIdList.add(storageAndRepositoryId);
                            }
                        }
                    }
                } catch (Exception ex) {
                    log.error("group repository resolvePathTraversal storageId: [{}] repositoryId [{}] error：[{}]", sId, rId, ExceptionUtils.getStackTrace(ex));
                }
            }
        }
        log.info("Repository [{}] [{}] storageAndRepositoryIdList [{}]", repository.getStorage().getId(), repository.getId(), String.join(",", storageAndRepositoryIdList));
        return storageAndRepositoryIdList;
    }

    public boolean isRepositoryResolvable(Repository repository) {
        final boolean isInService = repository.isInService();
        if (!isInService) {
            log.info("- Repository [{}] is not in service, skipping...",
                    repository.getStorageIdAndRepositoryId());
            return false;
        }
        return true;
    }
}
