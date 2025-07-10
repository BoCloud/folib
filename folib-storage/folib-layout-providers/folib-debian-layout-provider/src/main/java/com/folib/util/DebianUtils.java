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
package com.folib.util;

import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import com.google.common.io.ByteStreams;
import com.folib.artifact.coordinates.DebianCoordinates;
import com.folib.constant.DebianConstant;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.domain.DebianMetadata;
import com.folib.domain.DebianPackagesContext;
import com.folib.enums.ArchiveFormat;
import com.folib.enums.DebianArchiveFormat;
import com.folib.enums.DeltaIndexEventType;
import com.folib.event.DebianIndexEvent;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.storage.repository.Repository;
import com.folib.util.steam.SmarchiveInputStream;
import com.folib.validator.DpkgDependenciesValidator;
import com.folib.validator.DpkgDescriptionMd5Validator;
import com.folib.validator.DpkgFieldValidationItem;
import com.folib.validator.DpkgMandatoryFieldsValidator;
import com.folib.validator.DpkgPackageMetadataValidator;
import com.folib.validator.DpkgProvidesValidator;
import com.folib.validator.DpkgStatusValidator;
import com.folib.validator.MetadataValidationException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.io.IOUtils;
import org.apache.commons.io.input.BOMInputStream;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.util.Assert;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

/**
 * @author veadan
 **/
@Slf4j
public class DebianUtils {

    public static DebianMetadata extract(InputStream inputStream) {
        try (InputStream rawDeb = new BufferedInputStream(inputStream);
             ArchiveInputStream debContent = SmarchiveInputStream.realize(rawDeb);) {
            Pair<ArchiveEntry, ArchiveFormat> controlArchiveEntry = getControlArchiveEntry(debContent);
            if (controlArchiveEntry != null) {
                byte[] controlEntryBytes = IOUtils.toByteArray(debContent, ((controlArchiveEntry.getLeft()).getSize()));
                log.info("Finished extracting the compressed archive ('control.tar/.gz/.xz/.zst') dpkg control file from artifact");
                String controlContent = readControlEntry(controlEntryBytes, controlArchiveEntry.getRight());
                return StringUtils.isEmpty(controlContent) ? null : interpret(controlContent, getDpkgMetadataValidators());
            } else {
                return null;
            }
        } catch (Exception e) {
            log.error("Failed to validate metadata for artifact", e);
            throw new IllegalArgumentException(e.getMessage());
        }
    }

    private static Pair<ArchiveEntry, ArchiveFormat> getControlArchiveEntry(ArchiveInputStream archiveStream) throws IOException {
        ArchiveEntry archiveEntry;
        while ((archiveEntry = archiveStream.getNextEntry()) != null) {
            String entryName = archiveEntry.getName();
            if (!archiveStream.canReadEntryData(archiveEntry)) {
                continue;
            }
            for (ArchiveFormat format : ArchiveFormat.values()) {
                if (entryName.endsWith("control.tar" + format.getExtension())) {
                    return new ImmutablePair(archiveEntry, format);
                }
            }
        }
        return null;

    }

    private static String readControlEntry(byte[] controlArchiveContent, ArchiveFormat format) throws IOException {
        log.info("Un-tarring the dpkg control file contents");
        try (ByteArrayInputStream controlArchiveStream = new ByteArrayInputStream(controlArchiveContent);
             InputStream unZippedControlArchive = format.unzipControl(controlArchiveStream);
             TarArchiveInputStream unTarredControlArchive = new TarArchiveInputStream(unZippedControlArchive)) {
            TarArchiveEntry controlFileEntry = getControlFileEntry(unTarredControlArchive);
            if (controlFileEntry == null) {
                log.error("Unable to find the control file ('control') within the dpkg control archive ('control.tar/.gz/.xz/.zst')");
                return "";
            }
            long entryLength = controlFileEntry.getSize();
            return IOUtils.toString(new BOMInputStream(ByteStreams.limit(unTarredControlArchive, entryLength)), Charset.defaultCharset());
        }
    }

    private static TarArchiveEntry getControlFileEntry(TarArchiveInputStream controlStream) throws IOException {
        while (true) {
            TarArchiveEntry controlArchiveEntry;
            if ((controlArchiveEntry = controlStream.getNextTarEntry()) != null) {
                String entryName = controlArchiveEntry.getName();
                if (!"control".equals(entryName) && !"./control".equals(entryName) || !controlStream.canReadEntryData(controlArchiveEntry)) {
                    continue;
                }
                return controlArchiveEntry;
            }
            return null;
        }
    }

    public DebianMetadata getDpkgMetadata(Artifact artifact, String control) throws MetadataValidationException {
        Map<String, DpkgPackageMetadataValidator> dpkgMetadataValidators = getDpkgMetadataValidators();
        DebianMetadata metadata = interpret(control, dpkgMetadataValidators);
        writeArtifactPath(metadata, artifact);
        return metadata;
    }

//    public void writeArtifactPath(DebianMetadata metadata, Artifact artifact) {
//        appendOrReplace(metadata, "Filename", metadata.artifactRelativePath, artifact.getArtifactPath());
//        metadata.artifactRelativePath = artifact.getArtifactPath();
//    }


    public static DebianMetadata interpret(String control, Map<String, DpkgPackageMetadataValidator> dpkgMetadataValidators) {
        control = StringUtils.stripEnd(control, "\n\r");
        control = StringUtils.stripStart(control, "\n\r");
        control = control.trim();
        String[] controlRows = control.split("\\n(?!\\s|\\t)");
        DebianMetadata debianSpec = new DebianMetadata();
        ArrayList<String> rows = Lists.newArrayList(controlRows);
        debianSpec.controlFileContent = control;
//        debianSpec.artifactRelativePath = getAndValidate("Filename", rows, dpkgMetadataValidators);
//        debianSpec.sha1 = getAndValidate("SHA1", rows, dpkgMetadataValidators);
        debianSpec.packageName = getAndValidate("Package", rows, dpkgMetadataValidators);
        debianSpec.version = getAndValidate("Version", rows, dpkgMetadataValidators);
//        debianSpec.sha256 = getAndValidate("SHA256", rows, dpkgMetadataValidators);
//        debianSpec.md5sum = getAndValidate("MD5sum", rows, dpkgMetadataValidators);
        debianSpec.architecture = getAndValidate("Architecture", rows, dpkgMetadataValidators);
        debianSpec.size = getAndValidate("Installed-Size", rows, dpkgMetadataValidators);
        debianSpec.maintainer = getAndValidate("Maintainer", rows, dpkgMetadataValidators);
        debianSpec.section = getAndValidate("Section", rows, dpkgMetadataValidators);
        debianSpec.priority = getAndValidate("Priority", rows, dpkgMetadataValidators);
        debianSpec.website = getAndValidate("Homepage", rows, dpkgMetadataValidators);
        debianSpec.description = getAndValidate("Description", rows, dpkgMetadataValidators);
        debianSpec.depends = getListAndValidate("Depends", rows, dpkgMetadataValidators);
//        debianSpec.license = getLicense(getAndValidate("License", rows, dpkgMetadataValidators));
//        getAndValidate("Description-md5", rows, dpkgMetadataValidators);
//        getListAndValidate("Provides", rows, dpkgMetadataValidators);
//        getListAndValidate("Status", rows, "\\s", dpkgMetadataValidators);
        log.trace("Completed interpretation of raw dpkg metadata for package {}", debianSpec.packageName);
        return debianSpec;
    }


    private static String getLicense(String licenseRow) {
        if (!StringUtils.isNotBlank(licenseRow)) {
            return null;
        } else {
            return !licenseRow.contains("unknown") && !licenseRow.contains("no license listed") ? licenseRow : null;
        }
    }


    private static String getAndValidate(String name, List<String> lines, Map<String, DpkgPackageMetadataValidator> validators) throws MetadataValidationException {
        DpkgFieldValidationItem dpkgKeyValue = getDpkgKeyValue(name, lines);
        Assert.notNull(dpkgKeyValue, String.format("找不到制品的%s信息", name));
        if (validators.containsKey(name)) {
            DpkgPackageMetadataValidator validator = validators.get(name);
            validator.validate(dpkgKeyValue.getDpkgKey(), dpkgKeyValue.getDpkgValue());
        }
        return dpkgKeyValue.getDpkgValue();
    }

    private static List<String> getListAndValidate(String name, List<String> lines, Map<String, DpkgPackageMetadataValidator> dpkgMetadataValidators) throws MetadataValidationException {
        return getListAndValidate(name, lines, ",", dpkgMetadataValidators);
    }

    private static List<String> getListAndValidate(String name, List<String> lines, String separator, Map<String, DpkgPackageMetadataValidator> validators) throws MetadataValidationException {
        DpkgFieldValidationItem dpkgKeyValue = getDpkgKeyValue(name, lines);
        Assert.notNull(dpkgKeyValue, String.format("找不到制品的%s信息", name));
        String[] values = null;
        if (dpkgKeyValue.getDpkgValue() != null && StringUtils.isNotBlank(dpkgKeyValue.getDpkgValue())) {
            values = dpkgKeyValue.getDpkgValue().split(separator, -1);
        }
        if (validators.containsKey(name)) {
            DpkgPackageMetadataValidator validator = validators.get(name);
            validator.validate(dpkgKeyValue.getDpkgKey(), values);
        }
        return values != null ? Arrays.asList(values) : Collections.emptyList();
    }

    private static DpkgFieldValidationItem getDpkgKeyValue(String name, List<String> lines) {
        if (lines == null) {
            throw new IllegalArgumentException("lines cannot be null");
        }
        for (String line : lines) {
            if (line.startsWith(name)) {
                String extractedLine = line.replaceFirst(name + ":\\s*", "");
                extractedLine = StringUtils.isNotBlank(extractedLine) ? extractedLine : null;
                return new DpkgFieldValidationItem(name, extractedLine);
            }
        }
        return null;
    }


    public static boolean allAreBlank(String... args) {
        for (String arg : args) {
            if (!StringUtils.isBlank(arg)) {
                return false;
            }
        }
        return true;
    }

    public static boolean allAreNotBlank(String... args) {
        for (String arg : args) {
            if (!StringUtils.isNotBlank(arg)) {
                return false;
            }
        }
        return true;
    }

    protected static Map<String, DpkgPackageMetadataValidator> getDpkgMetadataValidators() {
        Map<String, DpkgPackageMetadataValidator> dpkgMetadataValidatorsMap = new HashMap<>();
        dpkgMetadataValidatorsMap.put("Depends", new DpkgDependenciesValidator(true));
        dpkgMetadataValidatorsMap.put("Package", new DpkgMandatoryFieldsValidator());
        dpkgMetadataValidatorsMap.put("Provides", new DpkgProvidesValidator());
        dpkgMetadataValidatorsMap.put("Status", new DpkgStatusValidator());
        dpkgMetadataValidatorsMap.put("Description-md5", new DpkgDescriptionMd5Validator());
        return dpkgMetadataValidatorsMap;
    }

    public static void appendOrReplace(DebianMetadata metadata, String fieldName, String oldValue, String newValue) {
        if (StringUtils.isNotBlank(newValue)) {
            if (StringUtils.isNotBlank(oldValue)) {
                metadata.controlFileContent = metadata.controlFileContent.replace(oldValue, newValue);
            } else {
                metadata.controlFileContent = metadata.controlFileContent.trim() + "\n" + fieldName + ": " + newValue;
            }
        }
    }

    public void writeArtifactPath(DebianMetadata metadata, Artifact artifact) {
        appendOrReplace(metadata, "Filename", metadata.artifactRelativePath, artifact.getArtifactPath());
        metadata.artifactRelativePath = artifact.getArtifactPath();
    }

    public static String getArchitectureFromPath(Artifact artifact) {
        return artifact.getArtifactPath().replaceFirst(".*binary-", "").replace("/Packages", "");
    }

    public static DebianPackagesContext createDebianPackagesContext(String distribution, Map.Entry<String, List<DebianIndexEvent>> eventsEntry) {
        DebianPackagesContext context = null;
        try {
            String[] compArch = (eventsEntry.getKey()).split(":");
            context = new DebianPackagesContext(distribution, compArch[0], compArch[1]);
        } catch (Exception e) {
            log.error("Failed to parse index context out of event mapping string {} for distribution {}: {}", distribution, eventsEntry.getKey(), e.getMessage());
        }
        return context;
    }

    public static String print(DebianPackagesContext packagesContext) {
        String dist = packagesContext.getDistribution();
        String comp = packagesContext.getComponent();
        String arch = packagesContext.getArchitecture();
        return dist != null && comp != null && arch != null ? dist + "/" + comp + "/" + arch : "[root]";
    }

    public static String trimTrailingSlashes(String str) {
        return str.replaceAll("/+$", "");
    }

    public static String trimLeadingSlashes(String input) {
        if (input == null || input.isEmpty()) {
            return input;
        }
        return input.replaceFirst("^/+", "");
    }

    public static String trimSlashes(String input) {
        if (input == null) {
            return null;
        }
        return input.replaceAll("^/+|/+$", "");
    }

    public static List<DebianMetadata> uniqueSortedMetadataList(Collection<DebianMetadata> dirtyList) {
        List<DebianMetadata> mds = Lists.newArrayList(ImmutableSet.copyOf(dirtyList));
        mds.sort(DebianMetadata::compareTo);
        return mds;
    }


    public static boolean shouldDeletePackagesOfContext(Collection<DebianMetadata> mds, DebianPackagesContext packagesContext, Set<String> forcedArchitectures) {
        return mds.isEmpty() && !forcedArchitectures.contains(packagesContext.getArchitecture());
    }

    public static void deleteTempFile(File tmpFile) {
        if (tmpFile != null) {
            try {
                if (tmpFile.exists() && !tmpFile.delete()) {
                    log.warn("Debian package indexer failed to delete (clean) temporary file: '{}' ", tmpFile.getPath());
                }
            } catch (Exception var2) {
                log.warn("Debian package indexer failed to delete (clean) temporary file: '{}'. {}", tmpFile.getPath(), var2.getMessage());
            }
        }

    }

    public static String pathToPackagesFile(DebianPackagesContext packagesContext, String filename) {
        String prefix = trimTrailingSlashes(packagesContext.getBinaryPath());
        return (prefix + "/" + filename);
    }

    public static List<String> pathsToPackagesFiles(DebianPackagesContext context) {
        List<String> paths = Lists.newArrayList();
        paths.add(pathToPackagesFile(context, "Packages.bz2"));
        paths.add(pathToPackagesFile(context, "Packages.gz"));
        paths.add(pathToPackagesFile(context, "Packages"));
        paths.add(pathToPackagesFile(context, "by-hash/"));
        return paths;
    }

    public static String getComponentPath(String dist, String comp) {
        return "dists/" + dist + "/" + comp + "/";
    }


    public static void setCoordinates(RepositoryPath repositoryPath, DebianCoordinates coordinates) {
        if (Objects.nonNull(repositoryPath) && Objects.nonNull(coordinates)) {
            try {
                if (!RepositoryFiles.isArtifact(repositoryPath)) {
                    return;
                }
                Artifact artifact = Optional.ofNullable(repositoryPath.getArtifactEntry())
                        .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                                coordinates));
                repositoryPath.setArtifact(artifact);
            } catch (Exception ex) {
                log.error("setCoordinates Exception {} repositoryPath {}", ExceptionUtils.getStackTrace(ex), repositoryPath);
            }
        }
    }

    public static DebianIndexEvent generateEvent(DebianCoordinates coordinates, Artifact artifact, DeltaIndexEventType type) {
        DebianIndexEvent event = new DebianIndexEvent();
        event.setEventType(type);
        event.setArtifact(artifact);
        event.setDistribution(coordinates.getDistribution());
        event.setComponent(coordinates.getComponent());
        event.setArchitecture(coordinates.getArchitecture());
        return event;

    }

    public static String getArrtString(String distribution,String component,String architecture){
        StringBuilder sb=new StringBuilder();
        sb.append(DebianConstant.ATTR_DISTRIBUTION).append("=").append(distribution).append(";");
        sb.append(DebianConstant.ATTR_COMPONENT).append("=").append(component).append(";");
        sb.append(DebianConstant.ATTR_ARCHITECTURE).append("=").append(architecture);
        return sb.toString();
    }

    public static String getComponentFromPackagesRepoPath(RepositoryPath packagesFileRepoPath) {
        RepositoryPath archRepoPath = packagesFileRepoPath.getParent();
        String errorMsg = ", Packages file need to be under dists/distribution/component path";
        if (archRepoPath == null) {
            throw new InvalidPathException(packagesFileRepoPath.getPath(), "Couldn't extract architecture from:" + packagesFileRepoPath + errorMsg);
        } else {
            RepositoryPath compRepoPath = archRepoPath.getParent();
            if (compRepoPath == null) {
                throw new InvalidPathException(packagesFileRepoPath.getPath(), "Couldn't extract component from:" + packagesFileRepoPath + errorMsg);
            } else {
                return compRepoPath.getName();
            }
        }
    }

    // 获取发行版的名字
    public static String getDistributionName(RepositoryPath sourceRepoPath) {

        if (sourceRepoPath == null) {
            return null;
        } else {
            RootRepositoryPath rootDirectory = sourceRepoPath.getFileSystem().getRootDirectory();
            RepositoryPath relativePath = rootDirectory.relativize(sourceRepoPath);;
            Path path = Paths.get(relativePath.getPath());
            if (!path.startsWith("dists") || path.getNameCount() < 2) {
                return null; // 不是有效的 Debian 发行版路径
            }
            // 获取 "dists" 后的第一个目录，即发行版名称
            return path.getName(1).toString();
        }
    }

    public static String getArchFromPackagesFileRepoPath(RepositoryPath packagesFileRepoPath) {
        RepositoryPath archRepoPath = packagesFileRepoPath.getParent();
        if (archRepoPath == null) {
            throw new InvalidPathException(packagesFileRepoPath.getPath(), "Couldn't extract architecture from:" + packagesFileRepoPath + ", Packages file need to be under dists/distribution/component path");
        } else {
            return archRepoPath.getName();
        }

    }

    public static List<DebianArchiveFormat> getPackagesArchiveFormats(){
        return List.of(DebianArchiveFormat.GZIP,DebianArchiveFormat.BZ2);
    }

    public  static Set<String> getDebianDefaultArchitectures(Repository repository){
        Set<String> debianDefaultArchitectures = new HashSet<>();
        debianDefaultArchitectures.add("amd64");
        debianDefaultArchitectures.add("arm64");
        debianDefaultArchitectures.add("aarch64");
        return debianDefaultArchitectures;

    }



}
