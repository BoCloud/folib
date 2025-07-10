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

import com.folib.domain.DebianPackagesContext;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Set;

/**
 * @author veadan
 * @since 2024-09-04 16:01
 */
@Slf4j
public class DebianPackagesMetadataIncrementalIndexer extends DebianPackagesMetadataIndexerBase {

    private final Set<String> addEvents;
    private final Set<String> deleteEvents;
    private final DebianPackagesContext context;
    private final String coordinates;
    private final Set<DebianPackagesContext> writtenPackages;
    private final List<String> packagesFilesToDelete;

    private final DebianIncrementalIndexer debianIncrementalIndexer;


    public DebianPackagesMetadataIncrementalIndexer(DebianIncrementalIndexer debianIncrementalIndexer, Repository repo, Set<String> addEvents, Set<String> deleteEvents, DebianPackagesContext context, Set<DebianPackagesContext> writtenPackages, List<String> packagesFilesToDelete, RepositoryPathResolver resolver, ArtifactManagementService artifactManagementService) {
        super(repo, resolver, artifactManagementService);
        this.addEvents = addEvents;
        this.deleteEvents = deleteEvents;
        this.context = context;
        this.writtenPackages = writtenPackages;
        this.packagesFilesToDelete = packagesFilesToDelete;
        this.coordinates = DebianUtils.print(context);
        this.debianIncrementalIndexer = debianIncrementalIndexer;
    }

    public void indexPackages() throws IOException {
        if (this.addEvents.isEmpty() && this.deleteEvents.isEmpty()) {
            log.debug("Index request for coordinates {} invoked, but no events were received. Skipping.", this.coordinates);
        } else {
            // 索引位置
            String packagesFilePath = DebianUtils.pathToPackagesFile(this.context, "Packages");
            log.debug("Indexing coordinates {} in repo {}. path being written is {}", this.coordinates, this.repo.getId(), packagesFilePath);
            this.writePackagesIndex(packagesFilePath);
            log.debug("Finished writing metadata to repo {} for coordinates {}", this.repo.getId(), this.coordinates);
        }
    }

    private void writePackagesIndex(String packagesFilePath) {
        try {
            log.debug("Streaming incremental index for Packages file at {}", packagesFilePath);
            File packagesFile = this.writePackagesMetadata(packagesFilePath);
            log.debug("Stream and write Packages file for coordinates {} to filesystem", this.coordinates);
            try {
                if (isMarkedForDeletion(packagesFile)) {
                    this.packagesFilesToDelete.addAll(DebianUtils.pathsToPackagesFiles(this.context));
                } else {
                    this.writePackagesFileContentToRepo(this.context, packagesFile);
                    this.writtenPackages.add(this.context);
                    log.debug("Deployment of Packages file for coordinates {} to repo", this.coordinates);
                }
            } finally {
                DebianUtils.deleteTempFile(packagesFile);
            }
        } catch (Exception e) {
            log.error("Error writing Packages index for " + this.coordinates + ": " + e.getMessage(), e);
        }

    }

    // 写临时文件
    private File writePackagesMetadata(String packagesFilePath) throws IOException {
        File tempPackagesFile = this.getTempPackagesFilePath(this.debianIncrementalIndexer.tempPath);
        try (InputStream currentPackagesFile = this.debianIncrementalIndexer.getPackageByRepo(this.repo, packagesFilePath)) {
            DebianIndexIncrementalFilter indexFilter = new DebianIndexIncrementalFilter(this.addEvents, this.deleteEvents);
            IncrementalIndexStreamer indexStream = new IncrementalIndexStreamer(currentPackagesFile, tempPackagesFile, indexFilter);
            indexStream.write();
        } catch (Throwable e) {
            log.error("Error writing Packages index for " + this.coordinates + ": " + e.getMessage(), e);
        }
        return tempPackagesFile;
    }


    public static boolean isMarkedForDeletion(File packagesFile) {
        if (packagesFile != null && packagesFile.exists() && packagesFile.length() > 1L) {
            return false;
        } else {
            log.debug("It is being marked for deletion.");
            return true;
        }
    }


}
