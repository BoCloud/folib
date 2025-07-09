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
package com.folib.cache;

import com.folib.domain.Artifact;
import com.folib.domain.DebianMetadata;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import com.folib.validator.MetadataValidationException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Objects;

/**
 * @author veadan
 * @since 2024-09-03 17:33
 */
@Slf4j
@Component
public class DebianPackagesMetadataFsCache implements DebianPackagesMetadataCache {

    @Value("${folib.temp}")
    private String basePath;
    private String tmpBasePath;
    @Resource
    private RepositoryPathResolver repositoryPathResolver;


    @PostConstruct
    public void initTmpPath() {
        this.tmpBasePath = this.basePath + "/debian";
        try {
            FileUtils.forceMkdir(new File(this.basePath));
            FileUtils.forceMkdir(new File(this.tmpBasePath));
            log.debug("Debian metadata fs cache inited at path {}", this.basePath);
        } catch (IOException e) {
            log.warn("Can't create Debian metadata cache at path {}: {}", this.basePath, e.getMessage());
        }
    }

    public DebianMetadata get(Artifact artifact, Repository repo) {
        DebianMetadata metadata;
        String artifactPath = artifact.getArtifactPath();
        log.debug("Retrieving metadata for artifact {} from cache", artifactPath);
        try (InputStream controlStream = this.get(artifactPath)) {
            if (Objects.isNull(controlStream)) {
                return null;
            }
            log.debug("Cache retrieval of metadata for artifact {} took ", artifactPath);
            String controlFileContent = IOUtils.toString(controlStream, StandardCharsets.UTF_8);
            log.debug("Read control file for artifact {} into buffer", artifactPath);
            metadata = this.extract(artifact, controlFileContent);
            log.debug("Extraction of metadata from cached control file for artifact {}", artifactPath);
            if (metadata != null && metadata.packageName != null) {
                return null;
            }
            log.debug("Metadata corrupted in cache for artifact {}.", artifactPath);
            this.remove(artifactPath);
            return metadata;
        } catch (Exception e) {
            log.error("Metadata extract failed", e);
            throw new RuntimeException(e.getMessage());
        }
    }

    protected DebianMetadata extract(Artifact artifact, String controlFileContent) throws MetadataValidationException {
        return new DebianUtils().getDpkgMetadata(artifact, controlFileContent);
    }

    @Override
    public void put(Artifact artifact, DebianMetadata metadata) {
        String artifactPath = repositoryPathResolver.resolve(artifact.getStorageId(), artifact.getRepositoryId(), artifact.getArtifactPath()).getArtifactPath();
        String cachePath = this.createCacheFullPath(artifactPath);
        log.debug("Caching metadata for artifact {}", artifactPath);
        File tempCacheFile = null;
        try {
            tempCacheFile = this.saveFileToTempDir(metadata, artifactPath);
            if (FileUtils.sizeOf(tempCacheFile) > 0L) {
                File target = FileUtils.getFile(cachePath);
                if (target.exists()) {
                    FileUtils.forceDelete(target);
                }
                FileUtils.moveFile(tempCacheFile, new File(cachePath));
            } else {
                log.warn("Cache file '{}' created with 0 bytes", this.tmpBasePath);
            }
        } catch (IOException var12) {
            log.warn("Failed to create metadata for artifact: '{}'", artifactPath, var12);
        } finally {
            FileUtils.deleteQuietly(tempCacheFile);
        }

    }

    public void remove(String path) {
        File cacheFilePath = new File(this.createCacheFullPath(path));
        if (cacheFilePath.exists()) {
            try {
                log.debug("Deleting cache path {}", path);
                FileUtils.forceDelete(cacheFilePath);
            } catch (IOException e) {
                log.warn("Can't delete cache file: '{}' --> {}", path, e.getMessage());
            }
        }
    }

    private InputStream get(String path) {
        String fullPath = this.createCacheFullPath(path);
        try {
            File file = new File(fullPath);
            if (file.exists()) {
                log.debug("Found file in cache path {}", path);
                return Files.newInputStream(Paths.get(fullPath));
            } else {
                log.debug("Path {} in cache not found", path);
                return null;
            }
        } catch (IOException e) {
            log.debug("Failed to read cache file: {}", fullPath, e);
            return null;
        }
    }

    private String createCacheFullPath(String path) {
        String basePath = DebianUtils.trimTrailingSlashes(this.basePath);
        return basePath + "/" + path;
    }

    private String createTempCacheFullPath(String path) {
        String tmpBasePath = DebianUtils.trimTrailingSlashes(this.tmpBasePath);
        return tmpBasePath + "/" + path;
    }


    private File saveFileToTempDir(DebianMetadata metadata, String artifactPath) throws IOException {
        File file = new File(this.createTempCacheFullPath(artifactPath));
        FileUtils.write(file, metadata.controlFileContent, StandardCharsets.UTF_8);
        return file;
    }

}
