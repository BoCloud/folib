/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.storage.metadata;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.folib.artifact.MavenArtifactUtils;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryPolicyEnum;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.repository.metadata.Metadata;
import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.apache.maven.index.artifact.Gav;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.javatuples.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author veadan
 */
@Component
public class MavenSnapshotManager {

    public static final String TIMESTAMP_FORMAT = "yyyyMMdd.HHmmss";

    private static final Logger logger = LoggerFactory.getLogger(MavenSnapshotManager.class);

    private static final List<String> EXTENSION_LIST = Lists.newArrayList(".jar", ".war", ".ear");

    @Inject
    private MavenMetadataManager mavenMetadataManager;

    public MavenSnapshotManager() {
    }

    public void deleteTimestampedSnapshotArtifacts(RepositoryPath basePath,
                                                   Versioning versioning,
                                                   int numberToKeep,
                                                   int keepPeriod)
            throws IOException,
            XmlPullParserException {
        Repository repository = basePath.getRepository();
        if (!Files.exists(basePath)) {
            logger.warn("Removal of timestamped Maven snapshot artifact: {} not exists.", basePath);
            return;
        }

        logger.info("Removal of timestamped Maven snapshot artifact {} in '{}:{}'.",
                basePath, repository.getStorage().getId(), repository.getId());

        Pair<String, String> artifactGroup = MavenArtifactUtils.getDirectoryGA(basePath);
        String artifactGroupId = artifactGroup.getValue0();
        String artifactId = artifactGroup.getValue1();

        if (versioning.getVersions().isEmpty()) {
            return;
        }

        for (String version : versioning.getVersions()) {
            if (!ArtifactUtils.isSnapshot(version)) {
                continue;
            }
            RepositoryPath versionDirectoryPath = basePath.resolve(ArtifactUtils.toSnapshotVersion(version));
            if (!removeTimestampedSnapshot(versionDirectoryPath, numberToKeep, keepPeriod)) {
                continue;
            }
            VersionCollector versionCollector = new VersionCollector();
            List<SnapshotVersion> snapshotVersions = versionCollector.collectTimestampedSnapshotVersions(versionDirectoryPath);
            Versioning snapshotVersioning = versionCollector.generateSnapshotVersions(snapshotVersions);
            if (Objects.nonNull(snapshotVersioning) && CollectionUtils.isEmpty(snapshotVersioning.getVersions()) && CollectionUtils.isEmpty(snapshotVersioning.getSnapshotVersions())) {
                RepositoryFiles.delete(versionDirectoryPath, true);
                logger.info("Removal of timestamped Maven snapshot artifact [{}] path", versionDirectoryPath);
                List<Integer> artifactCountList = Lists.newArrayList();
                List<RepositoryPath> deleteRepositoryPathList = Lists.newArrayList();
                RepositoryPath versionDirectoryPathParent = versionDirectoryPath.getParent();
                try (Stream<Path> pathStream = Files.walk(versionDirectoryPathParent)) {
                    pathStream
                            .forEach(p -> {
                                if (!isSameFile(versionDirectoryPathParent, p)) {
                                    if (isArtifact(p)) {
                                        artifactCountList.add(1);
                                    } else if (!isChecksum(p)) {
                                        deleteRepositoryPathList.add((RepositoryPath) p);
                                    }
                                }
                            });
                }
                if (CollectionUtils.isEmpty(artifactCountList)) {
                    for (RepositoryPath deleteRepositoryPath : deleteRepositoryPathList) {
                        try {
                            RepositoryFiles.delete(deleteRepositoryPath, true);
                            logger.info("Removal of timestamped Maven snapshot artifact [{}] parent path", versionDirectoryPathParent);
                        } catch (Exception ex) {
                            logger.warn(ExceptionUtils.getStackTrace(ex));
                        }
                    }
                }
                continue;
            }
            logger.info("Generate snapshot versioning metadata for {}.", versionDirectoryPath);
            mavenMetadataManager.generateLastSnapshotVersioningMetadata(artifactGroupId,
                    artifactId,
                    versionDirectoryPath,
                    version,
                    true);
        }
    }

    private boolean removeTimestampedSnapshot(RepositoryPath basePath,
                                              int numberToKeep,
                                              int keepPeriod)
            throws IOException,
            XmlPullParserException {
        Metadata metadata = mavenMetadataManager.readMetadata(basePath);

        if (metadata == null || metadata.getVersioning() == null) {
            return false;
        }

        List<String> removeVersionList = getRemovableTimestampedSnapshots(basePath, metadata, numberToKeep, keepPeriod);

        if (CollectionUtils.isEmpty(removeVersionList)) {
            return false;
        }
        List<Metadata> removeMetadataList = Lists.newArrayList();
        Metadata removeMetadata = null;
        for (String removeVersion : removeVersionList) {
            removeMetadata = new Metadata();
            removeMetadata.setArtifactId(metadata.getArtifactId());
            removeMetadata.setVersion(removeVersion);
            removeMetadataList.add(removeMetadata);
        }
        try (final DirectoryStream<Path> directoryStream = Files.newDirectoryStream(basePath)) {
            for (Path path : directoryStream) {
                if (!Files.isRegularFile(path)) {
                    continue;
                }
                RepositoryPath repositoryPath = (RepositoryPath) path;
                if (!RepositoryFiles.isArtifact(repositoryPath) ||
                        RepositoryFiles.isChecksum(repositoryPath) || RepositoryFiles.isArtifactMetadata(repositoryPath)) {
                    continue;
                }
                Gav gav = MavenArtifactUtils.convertPathToGav(repositoryPath);
                if (Objects.isNull(gav)) {
                    continue;
                }
                if (removeMetadataList.stream().noneMatch(item -> item.getArtifactId().equals(gav.getArtifactId()) && item.getVersion().equals(gav.getVersion()))) {
                    continue;
                }

                try {
                    RepositoryFiles.delete(repositoryPath, true);
                    String filename = repositoryPath.getFileName().toString();
                    logger.info("SnapshotRepositoryPath [{}] is removed", repositoryPath.toString());
                    RepositoryPath pomRepositoryPath = repositoryPath.resolveSibling(filename.replace("." + FilenameUtils.getExtension(filename), ".pom"));
                    if (Files.exists(pomRepositoryPath)) {
                        RepositoryFiles.delete(pomRepositoryPath, true);
                    }
                } catch (IOException ex) {
                    logger.error(ex.getMessage(), ex);
                }
            }
        }
        return true;
    }

    /**
     * To get list of removable timestamped snapshots
     *
     * @param basePath     basePath
     * @param metadata     type Metadata
     * @param numberToKeep type int
     * @param keepPeriod   type int
     * @return type List<String>
     */
    private List<String> getRemovableTimestampedSnapshots(RepositoryPath basePath, Metadata metadata,
                                                          int numberToKeep,
                                                          int keepPeriod) throws IOException {
        List<SnapshotVersionDecomposition> snapshots = Lists.newArrayList();

        List<String> removeVersionList = Lists.newArrayList();

        String lastVersion = "lastVersion", metadataLastVersion = "metadataLastVersion", version = metadata.getVersion();
        VersionCollector versionCollector = new VersionCollector();
        List<SnapshotVersion> snapshotVersions = versionCollector.collectTimestampedSnapshotVersions(basePath);
        //获取最新的快照版本，maven-metadata.xml中只保存最新的快照版本号
        String versionPrefix = version.replace(RepositoryPolicyEnum.SNAPSHOT.name(), "");
        snapshotVersions = snapshotVersions.stream().filter(snapshotVersion -> snapshotVersion.getVersion().startsWith(versionPrefix)).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(snapshotVersions)) {
            lastVersion = snapshotVersions.get(snapshotVersions.size() - 1).getVersion();
            metadataLastVersion = String.format("%s%s-%s", versionPrefix, metadata.getVersioning().getSnapshot().getTimestamp(), metadata.getVersioning().getSnapshot().getBuildNumber());
            logger.info("SnapshotBasePath [{}] lastVersion [{}] currentLastVersion [{}]", basePath.toString(), lastVersion, metadataLastVersion);
        }
        if (!lastVersion.equals(metadataLastVersion)) {
            return removeVersionList;
        }

        for (SnapshotVersion snapshotVersion : snapshotVersions) {
            SnapshotVersionDecomposition snapshotVersionDecomposition = SnapshotVersionDecomposition.of(snapshotVersion.getVersion());
            if (SnapshotVersionDecomposition.INVALID.equals(snapshotVersionDecomposition)) {
                logger.warn("SnapshotBasePath [{}] received invalid snapshot version {}", basePath.toString(), snapshotVersionDecomposition);
                continue;
            }
            if (!snapshots.contains(snapshotVersionDecomposition)) {
                snapshots.add(snapshotVersionDecomposition);
            }
        }
        if (CollectionUtils.isEmpty(snapshots)) {
            return removeVersionList;
        }
        logger.info("SnapshotBasePath [{}] snapshots [{}]", basePath.toString(), JSONObject.toJSONString(snapshots));
        if (numberToKeep > 0 && snapshots.size() > 1 && snapshots.size() > numberToKeep) {
            snapshots.forEach(v -> {
                if (removeVersionList.size() < snapshots.size() - numberToKeep) {
                    logger.info("SnapshotBasePath [{}] removeVersionList add [{}]", basePath.toString(), v.getVersion());
                    removeVersionList.add(v.getVersion());
                }
            });
        } else if (numberToKeep == 0 && keepPeriod > 0) {
            snapshots.forEach(v -> {
                try {
                    Date snapshotVersionDate = DateUtil.parse(v.getTimestamp(), DatePattern.createFormatter(TIMESTAMP_FORMAT));
                    Calendar calendar = Calendar.getInstance();
                    calendar.setTime(snapshotVersionDate);
                    calendar.add(Calendar.DAY_OF_MONTH, keepPeriod);
                    Date keepDate = calendar.getTime();
                    if (DateUtil.date().after(keepDate)) {
                        logger.info("SnapshotBasePath [{}] removeVersionList add [{}]", basePath.toString(), v.getVersion());
                        removeVersionList.add(v.getVersion());
                    }
                } catch (Exception e) {
                    logger.error(e.getMessage(), e);
                }
            });
        }
        logger.info("SnapshotBasePath [{}] removeVersionList [{}]", basePath.toString(), JSONObject.toJSONString(removeVersionList));
        return removeVersionList;
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
