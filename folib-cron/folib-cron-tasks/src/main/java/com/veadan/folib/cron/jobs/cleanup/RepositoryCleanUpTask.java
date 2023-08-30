package com.veadan.folib.cron.jobs.cleanup;

import cn.hutool.extra.spring.SpringUtil;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;

import java.util.concurrent.Callable;

/**
 * @author leipenghui
 **/
@Slf4j
public class RepositoryCleanUpTask implements Callable<String> {

    private String storageId;
    private String repositoryId;
    private String cleanDay;
    private String path;

    public RepositoryCleanUpTask(String storageId, String repositoryId, String cleanDay, String path) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        this.cleanDay = cleanDay;
        this.path = path;
    }

    @Override
    public String call() throws Exception {
        CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry = SpringUtil.getBean(CleanupArtifactsProviderRegistry.class);
        ConfigurationManager configurationManager = SpringUtil.getBean(ConfigurationManager.class);
        Repository repository = configurationManager.getRepository(storageId, repositoryId);
        String dockerLayout = "Docker", cleanupRepositoryType = "GENERAL";
        if (dockerLayout.equalsIgnoreCase(repository.getLayout())) {
            cleanupRepositoryType = "DOCKER";
        }
        CleanupArtifactsProvider cleanupArtifactsProvider = cleanupArtifactsProviderRegistry.getProvider(cleanupRepositoryType);
        return cleanupArtifactsProvider.cleanup(storageId, repositoryId, path, cleanDay);
    }
}
