package com.veadan.folib.storage.metadata;

import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.services.ArtifactMetadataService;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

/**
 * @author veadan
 */
@Component
public class MavenArtifactManager {

    public static final String TIMESTAMP_FORMAT = "yyyyMMdd.HHmmss";

    private static final Logger logger = LoggerFactory.getLogger(MavenArtifactManager.class);

    @Inject
    private ArtifactMetadataService artifactMetadataService;

    public MavenArtifactManager() {
    }

    public void deleteArtifacts(RepositoryPath basePath,
                                Versioning versioning,
                                int numberToKeep,
                                int keepPeriod)
            throws IOException,
            XmlPullParserException {
        Repository repository = basePath.getRepository();
        if (!Files.exists(basePath)) {
            logger.warn("Removal of Maven artifact: {} not exists.", basePath);
            return;
        }

        logger.info("Removal of Maven artifact {} in '{}:{}'.",
                basePath, repository.getStorage().getId(), repository.getId());

        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(basePath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();

        if (versioning.getVersions().isEmpty()) {
            return;
        }
        int size = versioning.getVersions().size();
        logger.info("Remove maven artifact job storageId [{}] repositoryId [{}] artifactGroupId [{}] artifactId [{}] version count [{}] numberToKeep [{}] versions [{}]", basePath.getStorageId(), basePath.getRepositoryId(), artifactGroupId, artifactId, size, numberToKeep, String.join(",", versioning.getVersions()));
        if (numberToKeep > 0 && size > 1 && size > numberToKeep) {
            List<String> versions = versioning.getVersions().subList(0, size - numberToKeep);
            logger.info("Remove maven artifact job storageId [{}] repositoryId [{}] artifactGroupId [{}] artifactId [{}] version count [{}] numberToKeep [{}] delete [{}]", basePath.getStorageId(), basePath.getRepositoryId(), artifactGroupId, artifactId, size, numberToKeep, String.join(",", versions));
            for (String version : versions) {
                handlerArtifact(basePath, 1, numberToKeep, keepPeriod, version, artifactGroupId, artifactId);
            }
        } else if (numberToKeep == 0 && keepPeriod > 0) {
            for (String version : versioning.getVersions()) {
                handlerArtifact(basePath, 2, numberToKeep, keepPeriod, version, artifactGroupId, artifactId);
            }
        }
    }

    private void handlerArtifact(RepositoryPath basePath, int type, int numberToKeep, int keepPeriod, String version, String artifactGroupId, String artifactId) {
        RepositoryPath versionDirectoryPath = null;
        try {
            if (StringUtils.isBlank(version)) {
                return;
            }
            if (ArtifactUtils.isSnapshot(version)) {
                versionDirectoryPath = basePath.resolve(ArtifactUtils.toSnapshotVersion(version));
            } else {
                versionDirectoryPath = basePath.resolve(version);
            }
            String artifactPath = RepositoryFiles.relativizePath(versionDirectoryPath.getParent());
            if (type == 1) {
                logger.info("Delete maven artifact path [{}]", versionDirectoryPath.toString());
                RepositoryFiles.delete(versionDirectoryPath, true);
            } else if (type == 2) {
                RepositoryPath versionDirectoryPathParent = versionDirectoryPath.getParent();
                try (Stream<Path> pathStream = Files.walk(versionDirectoryPath)) {
                    pathStream
                            .forEach(p -> {
                                try {
                                    if (!isSameFile(versionDirectoryPathParent, p) && compareTime((RepositoryPath) p, keepPeriod)) {
                                        logger.info("Delete maven artifact path [{}]", p.toString());
                                        RepositoryFiles.delete((RepositoryPath) p, true);
                                    }
                                } catch (Exception e) {
                                    logger.error(e.getMessage(), e);
                                }
                            });
                }
            }
            logger.info("Generate maven artifact versioning metadata for storageId {} repositoryId {} artifactPath {}.", basePath.getStorageId(), basePath.getRepositoryId(), artifactPath);
            artifactMetadataService.rebuildMetadata(basePath.getStorageId(), basePath.getRepositoryId(), artifactPath);
        } catch (Exception ex) {
            logger.error("Generate maven artifact versioning metadata for {}. error [{}]", versionDirectoryPath, ExceptionUtils.getStackTrace(ex));
        }
    }

    private boolean compareTime(RepositoryPath repositoryPath, int minusDays) {
        boolean confirmDelete = false;
        LocalDateTime updateDateTime = null;
        try {
            Artifact artifact = repositoryPath.getArtifactEntry();
            if (Objects.nonNull(artifact)) {
                //获取仓库下制品最近使用时间比较
                updateDateTime = artifact.getLastUsed();
            }
        } catch (Exception ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
        if (Objects.isNull(updateDateTime)) {
            updateDateTime = getFileUpdateTime(repositoryPath);
        }
        logger.info("RepositoryPath [{}] updateDateTime [{}]", repositoryPath.toString(), updateDateTime);
        if (Objects.isNull(updateDateTime)) {
            return true;
        }
        if (!LocalDateTime.now().minusDays(minusDays).isBefore(updateDateTime)) {
            confirmDelete = true;
        }
        return confirmDelete;
    }

    private LocalDateTime getFileUpdateTime(RepositoryPath repositoryPath) {
        LocalDateTime lastModifiedDateTime = null;
        try {
            BasicFileAttributes attributes = Files.readAttributes(repositoryPath, BasicFileAttributes.class);
            FileTime fileTime = attributes.lastModifiedTime();
            // 将FileTime转换为Instant
            Instant instant = fileTime.toInstant();
            // 将Instant转换为LocalDateTime
            lastModifiedDateTime = instant.atZone(ZoneId.of("Asia/Shanghai")).toLocalDateTime();
        } catch (IOException ex) {
            logger.error(ExceptionUtils.getStackTrace(ex));
        }
        return lastModifiedDateTime;
    }

    protected boolean isArtifact(Path path) {
        try {
            return Boolean.TRUE.equals(RepositoryFiles.isArtifact((RepositoryPath) path));
        } catch (IOException e) {
            logger.error("Failed to read Path attributes for [{}]", path, e);
            return false;
        }
    }

    protected boolean isChecksum(Path path) {
        try {
            return Boolean.TRUE.equals(RepositoryFiles.isChecksum((RepositoryPath) path));
        } catch (IOException e) {
            logger.error("Failed to read Path attributes for [{}]", path, e);
            return false;
        }
    }

    protected boolean isSameFile(Path path1, Path path2) {
        try {
            return Files.isSameFile(path1, path2);
        } catch (IOException e) {
            logger.error("Failed to read Path attributes for [{}] [{}] [{}]", path1, path2, e);
            return false;
        }
    }
}
