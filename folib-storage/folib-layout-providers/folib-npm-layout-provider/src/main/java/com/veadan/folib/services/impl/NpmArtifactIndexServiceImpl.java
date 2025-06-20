package com.veadan.folib.services.impl;

import com.veadan.folib.components.DistributedLockComponent;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.enums.NpmIndexTypeEnum;
import com.veadan.folib.indexer.NpmPackageMetadataIndexer;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.NpmLayoutProvider;
import com.veadan.folib.services.NpmArtifactIndexService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Service;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

/**
 * @author leipenghui
 **/
@Slf4j
@Service
public class NpmArtifactIndexServiceImpl implements NpmArtifactIndexService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private DistributedLockComponent distributedLockComponent;

    @Inject
    private NpmPackageMetadataIndexer npmPackageMetadataIndexer;

    @Override
    public void rebuildIndex(String storageId, String repositoryId, String artifactPath) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        if (!NpmLayoutProvider.ALIAS.equals(repository.getLayout())) {
            log.warn("Trying to rebuild index of repository {} with unsupported layout {} ", repository.getId(),
                    repository.getLayout());
            return;
        }

        if (!RepositoryTypeEnum.HOSTED.getType().equals(repository.getType())) {
            return;
        }

        RepositoryPath repositoryBasePath = repositoryPathResolver.resolve(repository);
        if (artifactPath != null && artifactPath.trim().length() > 0) {
            repositoryBasePath = repositoryBasePath.resolve(artifactPath);
        }
        if (!Files.exists(repositoryBasePath)) {
            return;
        }
        String key = String.format("NpmMetadata_%s_%s", storageId, repositoryId);
        if (distributedLockComponent.lock(key, GlobalConstants.WAIT_LOCK_TIME * GlobalConstants.WAIT_LOCK_TIME)) {
            try {
                try (Stream<Path> pathStream = Files.list(repositoryBasePath)) {
                    pathStream.filter(Files::isDirectory)
                            // Skip directories which start with a dot (like, for example: .index)
                            .filter(this::isArtifactDirectory)
                            // Note: Sorting can be expensive:
                            .sorted()
                            .forEach(this::execute);
                } catch (IOException ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            } finally {
                distributedLockComponent.unLock(key);
            }
        }
    }

    @Override
    public void rebuildIndex(RepositoryPath repositoryPath) {
        try {
            rebuildIndex(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), RepositoryFiles.relativizePath(repositoryPath));
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
    }

    public boolean isArtifactDirectory(Path path) {
        if (!(path instanceof RepositoryPath)) {
            return false;
        }
        boolean flag = false;
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            boolean ignore = RepositoryFiles.isHidden(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath) || RepositoryFiles.isTrash(repositoryPath) || RepositoryFiles.isTemp(repositoryPath);
            if (ignore) {
                return false;
            }
            String relativizePath = RepositoryFiles.relativizePath(repositoryPath);
            String[] pathArr = relativizePath.split(GlobalConstants.SEPARATOR);
            if (pathArr.length <= 2) {
                flag = true;
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return flag;
    }

    private void execute(Path path) {
        try {
            RepositoryPath repositoryPath = (RepositoryPath) path;
            String artifactPath = RepositoryFiles.relativizePath(repositoryPath);
            if (artifactPath.startsWith(GlobalConstants.AT) && !artifactPath.contains(GlobalConstants.SEPARATOR)) {
                //scope
                try (Stream<Path> pathStream = Files.list(repositoryPath)) {
                    pathStream.filter(Files::isDirectory)
                            // Skip directories which start with a dot (like, for example: .index)
                            .filter(this::isArtifactDirectory)
                            // Note: Sorting can be expensive:
                            .sorted()
                            .forEach(this::execute);
                } catch (IOException ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
                return;
            }
            npmPackageMetadataIndexer.indexAsSystem(repositoryPath, NpmIndexTypeEnum.REINDEX);
        } catch (Exception ex) {
            log.error("Rebuild index error [{}]", ExceptionUtils.getStackTrace(ex));
        }
    }

    public Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }
}
