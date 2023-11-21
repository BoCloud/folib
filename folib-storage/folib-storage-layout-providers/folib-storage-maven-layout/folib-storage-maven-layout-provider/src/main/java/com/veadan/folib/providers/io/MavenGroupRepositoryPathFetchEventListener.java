package com.veadan.folib.providers.io;

import com.google.common.collect.Lists;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.providers.repository.GroupRepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.providers.repository.event.GroupRepositoryPathFetchEvent;
import com.veadan.folib.services.support.ArtifactRoutingRulesChecker;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.FutureTask;

/**
 * @author veadan
 */
@Component
public class MavenGroupRepositoryPathFetchEventListener {

    private static final Logger logger = LoggerFactory.getLogger(MavenGroupRepositoryPathFetchEventListener.class);

    @Inject
    private Maven2LayoutProvider maven2LayoutProvider;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private ArtifactRoutingRulesChecker artifactRoutingRulesChecker;

    @Inject
    private RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    private ThreadPoolTaskExecutor asyncFetchRemotePackageThreadPoolTaskExecutor;

    @AsyncEventListener
    public void handle(final GroupRepositoryPathFetchEvent event)
            throws IOException {
        RepositoryPath repositoryPath = event.getPath();
        if (!Maven2LayoutProvider.ALIAS.equals(repositoryPath.getRepository().getLayout())) {
            return;
        }

        if (!maven2LayoutProvider.requiresGroupAggregation(repositoryPath)) {
            return;
        }

        fetchInSubRepositories(repositoryPath);
    }

    /**
     * @see GroupRepositoryProvider#resolvePathTraversal(RepositoryPath)
     */
    private void fetchInSubRepositories(final RepositoryPath repositoryPath)
            throws IOException {
        Repository groupRepository = repositoryPath.getRepository();
        Storage storage = groupRepository.getStorage();
        List<FutureTask<Path>> futureTasks = Lists.newArrayList();
        FutureTask<Path> futureTask = null;
        MavenGroupRepositoryPathFetchTask mavenGroupRepositoryPathFetchTask = null;
        for (String storageAndRepositoryId : groupRepository.getGroupRepositories()) {
            String sId = ConfigurationUtils.getStorageId(storage.getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManager.getRepository(sId, rId);

            if (!subRepository.isInService()) {
                continue;
            }

            RepositoryPath resolvedPath = repositoryPathResolver.resolve(subRepository, repositoryPath);
            if (artifactRoutingRulesChecker.isDenied(groupRepository, resolvedPath)) {
                continue;
            }

            RepositoryProvider provider = repositoryProviderRegistry.getProvider(subRepository.getType());
            mavenGroupRepositoryPathFetchTask = new MavenGroupRepositoryPathFetchTask(provider, resolvedPath);
            futureTask = new FutureTask<>(mavenGroupRepositoryPathFetchTask);
            futureTasks.add(futureTask);
            asyncFetchRemotePackageThreadPoolTaskExecutor.submit(futureTask);
        }

        fetchPathsIntTask(futureTasks);
    }

    private void fetchPathsIntTask(final List<FutureTask<Path>> futureTasks) {
        futureTasks
                .forEach(action -> {
                    try {
                        action.get();
                    } catch (Exception e) {
                        logger.error(e.getMessage(), e);
                    }
                });
    }
}
