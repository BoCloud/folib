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

import com.google.common.collect.Lists;
import com.folib.storage.metadata.maven.comparators.MetadataVersionComparator;
import com.folib.storage.metadata.maven.comparators.SnapshotVersionComparator;
import com.folib.storage.metadata.maven.io.filters.ArtifactVersionDirectoryFilter;
import com.folib.storage.metadata.maven.versions.MetadataVersion;
import com.folib.storage.metadata.maven.visitors.ArtifactVersionDirectoryVisitor;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.handler.DefaultArtifactHandler;
import org.apache.maven.artifact.repository.metadata.Plugin;
import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.repository.metadata.Versioning;
import org.apache.maven.index.artifact.Gav;
import org.apache.maven.index.artifact.M2GavCalculator;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author stodorov
 */
public class VersionCollector {

    private static final Logger logger = LoggerFactory.getLogger(VersionCollector.class);

    private static final M2GavCalculator M2_GAV_CALCULATOR = new M2GavCalculator();

    public VersionCollectionRequest collectVersions(Path artifactBasePath)
            throws IOException {
        VersionCollectionRequest request = new VersionCollectionRequest();
        request.setArtifactBasePath(artifactBasePath);

        List<MetadataVersion> versions = new ArrayList<>();

        List<Path> versionPaths;
        try (DirectoryStream<Path> ds = Files.newDirectoryStream(artifactBasePath,
                new ArtifactVersionDirectoryFilter())) {
            versionPaths = Lists.newArrayList(ds);
            versionPaths = versionPaths.stream().filter(dir -> Files.isDirectory(dir)).collect(Collectors.toList());
        }

        // Add all versions
        for (Path versionDirectoryPath : versionPaths) {
            try {
                Path pomArtifactPath = getPomPath(artifactBasePath, versionDirectoryPath);

                // No pom, no metadata.
                if (pomArtifactPath != null) {
                    logger.info("pomArtifactPath [{}]", pomArtifactPath.toString());
                    if (!Files.exists(pomArtifactPath)) {
                        logger.info("pomArtifactPath [{}] not exists", pomArtifactPath.toString());
                        continue;
                    }
                    Model pom = getPom(pomArtifactPath);

                    BasicFileAttributes fileAttributes = Files.readAttributes(versionDirectoryPath,
                            BasicFileAttributes.class);

                    // TODO: This will not work for versionless POM-s which extend the version from a parent.
                    // TODO: If pom.getVersion() == null, walk the parents until a parent with
                    // TODO: a non-null version is found and use that as the version.
                    String version = pom.getVersion() != null ? pom.getVersion() :
                            (pom.getParent() != null ? pom.getParent().getVersion() : null);

                    if (version == null) {
                        continue;
                    }

                    if (ArtifactUtils.isSnapshot(version)) {
                        version = ArtifactUtils.toSnapshotVersion(version);
                    }

                    MetadataVersion metadataVersion = new MetadataVersion();
                    metadataVersion.setVersion(version);
                    metadataVersion.setCreatedDate(fileAttributes.lastModifiedTime());

                    versions.add(metadataVersion);

                    if (artifactIsPlugin(pom)) {
                        String name = pom.getName() != null ? pom.getName() : pom.getArtifactId();

                        // TODO: SB-339: Get the maven plugin's prefix properly when generating metadata
                        // TODO: This needs to be addressed properly, as it's not correct.
                        // TODO: This can be obtained from the jar's META-INF/maven/plugin.xml and should be read
                        // TODO: either via a ZipInputStream, or using TrueZip.
                        // String prefix = pom.getArtifactId().replace("maven-plugin", "").replace("-plugin$", "");

                        Plugin plugin = new Plugin();
                        plugin.setName(name);
                        plugin.setArtifactId(pom.getArtifactId());
                        plugin.setPrefix(PluginDescriptor.getGoalPrefixFromArtifactId(pom.getArtifactId()));

                        request.addPlugin(plugin);
                    }
                }
            } catch (XmlPullParserException | IOException e) {
                logger.error("POM file '{}' appears to be corrupt.", versionDirectoryPath.toAbsolutePath(), e);
            }
        }

        // 1.1 < 1.2 < 1.3 ....
        if (!versions.isEmpty()) {
            Collections.sort(versions, new MetadataVersionComparator());
        }

        request.setMetadataVersions(versions);
        request.setVersioning(generateVersioning(versions));

        return request;
    }

    private Path getPomPath(Path artifactBasePath,
                            Path versionDirectoryPath) {
        String version = versionDirectoryPath.getFileName().toString();
        if (!ArtifactUtils.isSnapshot(version)) {
            Path path = Paths.get(versionDirectoryPath.toAbsolutePath().toString(),
                    artifactBasePath.getFileName().toString() + "-" +
                            versionDirectoryPath.getFileName() + ".pom");
            return path;
        } else {
            // Attempt to get the latest available POM
//            List<String> filePaths = Arrays.asList(versionDirectoryPath.toFile()
//                    .list((dir, name) -> name.endsWith(".pom")));

            List<String> filePaths = null;

            try (Stream<Path> pathStream = Files.list(versionDirectoryPath)) {
                filePaths = pathStream.filter(f -> !Files.isDirectory(f) && f.getFileName().toString().endsWith(".pom")).map(p -> p.toAbsolutePath().toString()).collect(Collectors.toList());
            } catch (IOException ex) {
                logger.error("[{}] getPomPath error [{}]", this.getClass().getSimpleName(), ExceptionUtils.getStackTrace(ex));
                return null;
            }

            if (!filePaths.isEmpty()) {
                Collections.sort(filePaths);
                    return Paths.get(filePaths.get(filePaths.size() - 1));
            } else {
                return null;
            }
        }
    }

    /**
     * Get snapshot versioning information for every released snapshot
     *
     * @param artifactVersionPath
     * @throws IOException
     */
    public List<SnapshotVersion> collectTimestampedSnapshotVersions(Path artifactVersionPath)
            throws IOException {
        List<SnapshotVersion> snapshotVersions = new ArrayList<>();
        if (!Files.exists(artifactVersionPath)) {
            logger.warn("RepositoryPath [{}] not exists", artifactVersionPath);
            return snapshotVersions;
        }
        ArtifactVersionDirectoryVisitor artifactVersionDirectoryVisitor = new ArtifactVersionDirectoryVisitor();

        Files.walkFileTree(artifactVersionPath, artifactVersionDirectoryVisitor);

        for (Path filePath : artifactVersionDirectoryVisitor.getMatchingPaths()) {
            String unixBasedFilePath = FilenameUtils.separatorsToUnix(filePath.toString());
            Gav gav = M2_GAV_CALCULATOR.pathToGav(unixBasedFilePath);
            if (Objects.isNull(gav)) {
                continue;
            }
            Artifact artifact = new DefaultArtifact(gav.getGroupId(),
                    gav.getArtifactId(),
                    gav.getVersion(),
                    null,
                    gav.getExtension(),
                    gav.getClassifier(),
                    new DefaultArtifactHandler(gav.getExtension()));
            String name = filePath.getFileName().toString();
//            String name = FileUtil.getName(filePath.toString());
//            if (filePath instanceof S3Path) {
//                S3Path s3Path = (S3Path) filePath;
//                name = s3Path.getFileName().toString();
//            } else {
//                name = filePath.toFile().getName();
//            }

            SnapshotVersion snapshotVersion = MetadataHelper.createSnapshotVersion(artifact,
                    FilenameUtils.getExtension(name));

            snapshotVersions.add(snapshotVersion);
        }

        if (!snapshotVersions.isEmpty()) {
            Collections.sort(snapshotVersions, new SnapshotVersionComparator());
        }

        return snapshotVersions;
    }

    public Versioning generateVersioning(List<MetadataVersion> versions) {
        Versioning versioning = new Versioning();

        if (!versions.isEmpty()) {
            // Sort versions naturally (1.1 < 1.2 < 1.3 ...)
            Collections.sort(versions, new MetadataVersionComparator());
            for (MetadataVersion version : versions) {
                versioning.addVersion(version.getVersion());
            }

            // Sort versions naturally but consider creation date as well so that
            // 1.1 < 1.2 < 1.4 < 1.3 (1.3 is considered latest release because it was changed recently)
            // TODO: Sort this out as part of SB-333
            //Collections.sort(versions);
        }

        return versioning;
    }

    public Versioning generateSnapshotVersions(List<SnapshotVersion> snapshotVersionList) {
        Versioning versioning = new Versioning();

        if (!snapshotVersionList.isEmpty()) {
            versioning.setSnapshotVersions(snapshotVersionList);
        }

        return versioning;
    }

    private boolean artifactIsPlugin(Model model) {
        return model.getPackaging().equals("maven-plugin");
    }

    private Model getPom(Path filePath)
            throws IOException, XmlPullParserException {

        logger.info("filePath [{}]", filePath.toString());
        try (InputStream inputStream = Files.newInputStream(filePath)) {
            MavenXpp3Reader reader = new MavenXpp3Reader();
            return reader.read(inputStream);
        }

    }

}
