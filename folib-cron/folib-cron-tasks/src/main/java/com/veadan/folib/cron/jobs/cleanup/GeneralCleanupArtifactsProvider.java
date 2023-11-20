package com.veadan.folib.cron.jobs.cleanup;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class GeneralCleanupArtifactsProvider implements CleanupArtifactsProvider {

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        cleanupArtifactsProviderRegistry.addProvider("GENERAL", this);
        log.info("Registered cleanup repository cron job '{}' with alias '{}'.",
                getClass().getCanonicalName(), "GENERAL");
    }

    @Override
    public String cleanup(String storageId, String repositoryId, String path, String cleanDay) throws Exception {
        long tempDay = Long.parseLong(cleanDay);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, path);
        if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
            log.warn("Cleanup storageId {} repositoryId {} path {} file not exists", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isTrash(repositoryPath)) {
            log.info("Cleanup storageId {} repositoryId {} path {} is trash file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isTemp(repositoryPath)) {
            log.info("Cleanup storageId {} repositoryId {} path {} is temp file skip", storageId, repositoryId, path);
            return null;
        }
        if (RepositoryFiles.isChecksum(repositoryPath)) {
            log.info("Cleanup storageId {} repositoryId {} path {} is checksum file skip", storageId, repositoryId, path);
            return null;
        }
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (null == artifact || null == artifact.getLastUsed()) {
            log.warn("Cleanup storageId {} repositoryId {} path {} artifact not found", storageId, repositoryId, path);
            return null;
        }
        //获取仓库下制品最近使用时间做比较
        LocalDateTime localDateTime = artifact.getLastUsed();
        //保留N天的制品
        log.info("Cleanup storageId {} repositoryId {} artifact {} time {} current time {}", storageId, repositoryId, artifact.getArtifactPath(), localDateTime, LocalDateTime.now());
        if (!LocalDateTime.now().minusDays(tempDay).isBefore(localDateTime)) {
            try {
                log.info("Cleanup storageId {} repositoryId {} path {} do delete", storageId, repositoryId, repositoryPath.toString());
                artifactManagementService.delete(repositoryPath, true);
                RepositoryPath parent = null;
                try {
                    parent = repositoryPath.getParent();
                    if (Files.exists(parent) && !Files.isSameFile(repositoryPath.getRoot(), parent) && Files.list(parent).count() == 0) {
                        Files.deleteIfExists(parent);
                        log.info("Delete parent root path {}", parent.toString());
                    }
                } catch (Exception ex) {
                    log.warn("Cleanup storageId {} repositoryId {} parent path {} error {}", storageId, repositoryId, parent, ExceptionUtils.getStackTrace(ex));
                }
                return "ok";
            } catch (Exception e) {
                log.error("Cleanup storageId {} repositoryId {} path {} error {}", storageId, repositoryId, path, ExceptionUtils.getStackTrace(e));
                return "fail";
            }
        }
        return null;
    }
}
