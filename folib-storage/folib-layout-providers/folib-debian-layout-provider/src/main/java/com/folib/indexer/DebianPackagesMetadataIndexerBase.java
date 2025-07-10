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
package com.folib.indexer;

/**
 * @author veadan
 * @since 2024-09-04 14:46
 */

import com.folib.domain.DebianPackagesContext;
import com.folib.enums.DebianArchiveFormat;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.CompressorException;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

@Slf4j
public class DebianPackagesMetadataIndexerBase {

    private static final String ALGORITHM_MD5SUM = "MD5Sum";
    private static final String ALGORITHM_SHA1 = "SHA1";
    private static final String ALGORITHM_SHA256 = "SHA256";
    private final String tempPathPrefix = "temp";
    protected final Repository repo;

    private final RepositoryPathResolver resolver;
    private final ArtifactManagementService artifactManagement;

    public DebianPackagesMetadataIndexerBase(Repository repo, RepositoryPathResolver resolver, ArtifactManagementService artifactManagement) {
        this.repo = repo;
        this.resolver=resolver;
        this.artifactManagement=artifactManagement;
    }
    protected void writePackagesFileContentToRepo(DebianPackagesContext packagesContext, File packagesFile) throws IOException, CompressorException {
        List<DebianArchiveFormat> packagesArchiveFormats = Arrays.asList(DebianArchiveFormat.GZIP, DebianArchiveFormat.BZ2);
        log.debug("Deploying Debian Packages metadata for coordinates {}", DebianUtils.print(packagesContext));
        try (InputStream contentStream = Files.newInputStream(packagesFile.toPath())) {
            DebianArchiveFormat.writePackagesFile(packagesContext, "Packages", contentStream, this.repo, this.resolver,artifactManagement);
        }
        packagesArchiveFormats.forEach(format -> format.writePackagesIndexFile(this.repo, packagesContext, packagesFile, this.resolver,artifactManagement));
        log.debug("Finished deploying Debian Packages metadata for coordinates {}", DebianUtils.print(packagesContext));
    }
    // todo byhash 目录后续实现不影响存储库的正常使用
//    public void copyPackagesFilesToByHashHierarchy(Set<DebianPackagesContext> packagesContexts, List<Artifact> originPackagesFiles) {
//        try {
//            Set<Artifact> allArtifactSet = this.getArtifactsUnderTopMostCommonParentPath(packagesContexts);
//            packagesContexts.forEach(context -> {
//                long byHashStart = System.currentTimeMillis();
//                List<Artifact> originPackagesToCopy = originPackagesFiles.stream()
//                        .filter(packagesFile -> this.isPathInContext(context.getBinaryPath(), packagesFile.getArtifactPath()))
//                        .collect(Collectors.toList());
//                List<Artifact> oldHistoryByHashPackagesFiles = allArtifactSet.stream()
//                        .filter(packagesFile -> packagesFile.getArtifactPath().contains("by-hash"))
//                        .filter(byHashFile -> byHashFile.getArtifactPath().contains(context.getBinaryPath()))
//                        .collect(Collectors.toList());
//                if (!this.oldHistoryContainsNewIndexFiles(oldHistoryByHashPackagesFiles, originPackagesToCopy)) {
//                    if (!oldHistoryByHashPackagesFiles.isEmpty()) {
//                        this.deleteOldHistoryByHash(oldHistoryByHashPackagesFiles);
//                    }
//
//                    log.debug("Copying Packages indices into by-hash hierarchy of coordinates '{}'", DebianUtils.print(context));
//                    originPackagesToCopy.forEach(artifact -> this.copyToByHashFolder(context, artifact));
//                }
//
//                log.trace("Finished copying Packages indices into by-hash hierarchy for coordinates {}. took {}ms", DebianUtils.print(context), DpkgUtils.time(byHashStart));
//            });
//        } catch (Exception var4) {
//            log.error("by-hash copy failed due to: ", var4);
//        }
//    }
//
//    private boolean oldHistoryContainsNewIndexFiles(List<Artifact> oldHistoryByHashPackagesFiles, List<Artifact> originPackagesToCopy) {
//        List<String> originSha2 = originPackagesToCopy.stream().map(Artifact).collect(Collectors.toList());
//        List<String> oldHistorySha2 = oldHistoryByHashPackagesFiles.stream().map(Artifact::getSha256).collect(Collectors.toList());
//        return !Collections.disjoint(originSha2, oldHistorySha2);
//    }
//
//    private Set<Artifact> getArtifactsUnderTopMostCommonParentPath(Set<DebianPackagesContext> packagesContexts) {
//        String[] allPaths = packagesContexts.stream().map(DebianPackagesContext::getBinaryPath).toArray(String[]::new);
//        if (this.repo.getRepository().getType().equals(RepositoryTypeEnum.GROUP.getType())) {
//            Set<Artifact> allByHashArtifacts = Sets.newHashSet();
//            Arrays.stream(allPaths).forEach(pathToArch -> allByHashArtifacts.addAll(Sets.newHashSet(this.repo.findArtifacts(pathToArch + "/by-hash/", ""))));
//            return allByHashArtifacts;
//        } else {
//            String basePathToArtifacts = StringUtils.getCommonPrefix(allPaths);
//            basePathToArtifacts = basePathToArtifacts.substring(0, basePathToArtifacts.lastIndexOf("/") + 1);
//            log.debug("Initiating search to get all by-hash index artifacts under '{}'", basePathToArtifacts);
//            Iterable<Artifact> allArtifacts = this.repo.findArtifacts(basePathToArtifacts + "*/by-hash/", "*");
//            return Sets.newHashSet(allArtifacts);
//        }
//    }
//
//    private void deleteOldHistoryByHash(List<Artifact> artifactSetInFolder) {
//        DebianRepoWorkContext workContext = (DebianRepoWorkContext) this.repo.getWorkContext();
//        if (workContext == null) {
//            throw new IllegalStateException("Work context is null");
//        }
//        int packagesArchiveFormatsNumber = workContext.getPackagesArchiveFormats().size() + 1;
//        int historySize = workContext.getPackagesByHashHistoryCyclesToKeep() * packagesArchiveFormatsNumber;
//        this.deleteByHashOldHistoryIfExists(ALGORITHM_MD5SUM, historySize, packagesArchiveFormatsNumber, artifactSetInFolder);
//        this.deleteByHashOldHistoryIfExists(ALGORITHM_SHA1, historySize, packagesArchiveFormatsNumber, artifactSetInFolder);
//        this.deleteByHashOldHistoryIfExists(ALGORITHM_SHA256, historySize, packagesArchiveFormatsNumber, artifactSetInFolder);
//    }
//
//    private void deleteByHashOldHistoryIfExists(String algorithm, int historySize, int archiveFormatNumber, List<Artifact> artifacts) {
//        List<Artifact> artifactsUnderAlgorithmFolder = artifacts.stream()
//                .filter(artifact -> artifact.getPath().contains(algorithm))
//                .collect(Collectors.toList());
//        if (artifactsUnderAlgorithmFolder.size() >= historySize) {
//            artifactsUnderAlgorithmFolder.stream()
//                    .sorted(Comparator.comparingLong(Artifact::getLastModified))
//                    .limit(archiveFormatNumber)
//                    .forEach(this::deletePackagesIndexArtifact);
//        }
//    }
//
//    private void deletePackagesIndexArtifact(Artifact artifact) {
//        String path = artifact.getPath();
//        try {
//            this.repo.delete(path);
//        } catch (Exception var4) {
//            log.warn("Failed to delete Packages index at {}: {}", path, var4.getMessage());
//            log.debug("", var4);
//        }
//    }
//
//    private void copyToByHashFolder(DebianPackagesContext packagesContext, Artifact artifact) {
//        String packagesPath = artifact.getPath();
//        String fullPath = artifact.getRepoId() + "/" + packagesPath;
//        String md5Path = this.pathToByHashPackagesFile(packagesContext, ALGORITHM_MD5SUM, artifact.getMd5());
//        log.debug("Copying {} into by-hash folder at {}", fullPath, md5Path);
//        this.repo.copy(packagesPath, md5Path);
//        String sha1Path = this.pathToByHashPackagesFile(packagesContext, ALGORITHM_SHA1, artifact.getSha1());
//        log.debug("Copying {} into by-hash folder at {}", fullPath, sha1Path);
//        this.repo.copy(packagesPath, sha1Path);
//        String sha2Path = this.pathToByHashPackagesFile(packagesContext, ALGORITHM_SHA256, artifact.getSha256());
//        log.debug("Copying {} into by-hash folder at {}", fullPath, sha2Path);
//        this.repo.copy(packagesPath, sha2Path);
//    }
//
//    private String pathToByHashPackagesFile(DebianPackagesContext context, String subdir, String filename) {
//        if (context.isAutomaticLayout()) {
//            return String.format("%s/%s/%s/%s/%s/binary-%s/%s/%s/%s", "dists", context.getDistribution(), this.tempPathPrefix, context.getDistribution(), context.getComponent(), context.getArchitecture(), "by-hash", subdir, filename);
//        } else {
//            return "byHash" + this.tempPathPrefix + DebianUtils.getContextPath(context.getBinaryPath()) + "/" + subdir + "/" + filename;
//        }
//    }

    private boolean isPathInContext(String contextEntryPath, String path) {
        if (!path.contains(this.tempPathPrefix)) {
            return false;
        } else {
            String contextPath = contextEntryPath.startsWith("/") ? contextEntryPath : contextEntryPath.substring(contextEntryPath.indexOf("/"));
            int indexAfterTempFolder = path.indexOf(this.tempPathPrefix) + this.tempPathPrefix.length();
            String packageFileContext = path.substring(indexAfterTempFolder, path.lastIndexOf("/"));
            return contextPath.equals(packageFileContext);
        }
    }

    protected File getTempPackagesFilePath(String tempPath) throws IOException {
        Path tempDir = Paths.get(tempPath);
        return Files.createTempFile(tempDir, "deb", "metadata").toFile();
    }
}

