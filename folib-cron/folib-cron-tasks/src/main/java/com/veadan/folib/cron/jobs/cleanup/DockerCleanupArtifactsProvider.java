package com.veadan.folib.cron.jobs.cleanup;

import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ArtifactManagementService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.File;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class DockerCleanupArtifactsProvider implements CleanupArtifactsProvider {

    @Inject
    private CleanupArtifactsProviderRegistry cleanupArtifactsProviderRegistry;

    @Inject
    private ArtifactManagementService artifactManagementService;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        cleanupArtifactsProviderRegistry.addProvider("DOCKER", this);
        log.info("Registered cleanup repository cron job '{}' with alias '{}'.",
                getClass().getCanonicalName(), "DOCKER");
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
        if (!RepositoryFiles.isArtifact(repositoryPath)) {
            log.info("Cleanup storageId {} repositoryId {} path {} not is artifact file skip", storageId, repositoryId, path);
            return null;
        }
        String artifactPath = repositoryPath.toString();
        boolean isDockerVersion = repositoryPath.getFileName().toString().startsWith("sha256:") && !artifactPath.contains("blobs/sha256") && !artifactPath.contains("manifest/sha256");
        if (!isDockerVersion) {
            log.info("Cleanup storageId {} repositoryId {} path {} not a docker version file skip", storageId, repositoryId, path);
            return null;
        }
        log.info("Cleanup storageId {} repositoryId {} path {} find a docker version file", storageId, repositoryId, path);
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (null == artifact || null == artifact.getLastUsed()) {
            log.warn("Cleanup storageId {} repositoryId {} path {} artifact not found", storageId, repositoryId, path);
            return null;
        }
        RepositoryPath manifestRepositoryPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), repositoryPath.getParent().getParent().getFileName().toString() + File.separator + "manifest" + File.separator + repositoryPath.getFileName().toString());
        if (!Files.exists(manifestRepositoryPath)) {
            log.warn("Cleanup storageId {} repositoryId {} path {} manifest file not exists", storageId, repositoryId, manifestRepositoryPath.toString());
            return null;
        }
        Artifact manifestArtifact = manifestRepositoryPath.getArtifactEntry();
        if (null == manifestArtifact || null == manifestArtifact.getLastUsed()) {
            log.warn("Cleanup storageId {} repositoryId {} path {} manifest artifact not found", storageId, repositoryId, manifestRepositoryPath.toString());
            return null;
        }
        //获取仓库下制品最近使用时间做比较
        LocalDateTime tagTime = artifact.getLastUsed();
        LocalDateTime manifestTime = manifestArtifact.getLastUsed();
        //保留N天的制品
        log.info("Cleanup docker storageId {} repositoryId {} artifact {} time {} manifest time {} current time {}", storageId, repositoryId, artifact.getArtifactPath(), tagTime, manifestTime, LocalDateTime.now());
        if (!LocalDateTime.now().minusDays(tempDay).isBefore(tagTime) && !LocalDateTime.now().minusDays(tempDay).isBefore(manifestTime)) {
            try {
                RepositoryPath deleteRepositoryPath = repositoryPath.getParent();
                log.info("Cleanup docker version storageId {} repositoryId {} path {} do delete", storageId, repositoryId, deleteRepositoryPath.toString());
                artifactManagementService.delete(deleteRepositoryPath, true);
                RepositoryPath dockerImageRepositoryPath = deleteRepositoryPath.getParent();
                long count = Files.list(dockerImageRepositoryPath).count();
                if (count == 0) {
                    Files.delete(dockerImageRepositoryPath);
                    log.info("Cleanup docker image storageId {} repositoryId {} path {} do delete", storageId, repositoryId, dockerImageRepositoryPath.toString());
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
