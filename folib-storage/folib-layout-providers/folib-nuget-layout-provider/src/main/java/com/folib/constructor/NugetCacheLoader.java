package com.folib.constructor;

import com.folib.nuget.indexer.NugetMetadataExtractor;
import com.folib.nuget.indexer.model.NuSpecPackage;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.service.NugetCacheService;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.annotation.PostConstruct;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Stream;

/**
 * @author LingengMa
 * @date 2025/05/19 15:12
 * @Description:
 */


@Component
@Slf4j
public class NugetCacheLoader {
    private final ConfigurationManagementService configurationManagementService;

    private final RepositoryPathResolver repositoryPathResolver;

    private final NugetCacheService nugetCacheService;

    @Autowired
    public NugetCacheLoader(ConfigurationManagementService configurationManagementService, RepositoryPathResolver repositoryPathResolver, NugetCacheService nugetCacheService) {
        this.configurationManagementService = configurationManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.nugetCacheService = nugetCacheService;
    }


    @PostConstruct
    public void init() {
        log.info("Loading Nuget cache for all hosted Nuget repositories...");
        Map<String, Storage> storages = configurationManagementService.getConfiguration().getStorages();
        for (String storageId : storages.keySet()) {
            Storage storage = storages.get(storageId);
            if (storage == null || storage.getRepositories() == null) {
                continue;
            }
            for (String repositoryId : storage.getRepositories().keySet()) {
                Repository repository = storage.getRepositories().get(repositoryId);
                if (repository.isHostedRepository() && repository.getLayout().equalsIgnoreCase("nuget")) {
                    LoadCache(repository);
                }
            }
        }
    }

    public void LoadCache(Repository repository) {
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();
        NugetMetadataExtractor nugetMetadataExtractor = new NugetMetadataExtractor();
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository);
        try (Stream<Path> pathStream = Files.walk(repositoryPath)) {
            pathStream.filter(Files::isRegularFile)
                    .filter(path -> path.getFileName().toString().endsWith(".nupkg"))
                    .filter(path -> !path.toString().startsWith("."))
                    .forEach(path -> {
                        try (InputStream is = Files.newInputStream(path)) {
                            NuSpecPackage nuspec = nugetMetadataExtractor.extractNuspecFromStream(is);
                            nugetCacheService.cachePackage(repository, nuspec.getMetadata());
                        } catch (Exception e) {
                            log.warn("Failed to load nuget metadata from " + repositoryPath, e);
                        }
                    });
        } catch (Exception e) {
            log.error("Failed to load Nuget cache for repository: " + storageId + ":" + repositoryId, e);
        }
    }
}
