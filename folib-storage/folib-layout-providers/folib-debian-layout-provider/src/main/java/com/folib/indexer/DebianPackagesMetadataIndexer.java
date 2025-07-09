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

import com.folib.domain.DebianMetadata;
import com.folib.domain.DebianPackagesContext;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.Repository;
import com.folib.util.DebianUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.compressors.CompressorException;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * @author veadan
 * @since 2024-09-04 15:30
 */
@Slf4j
public class DebianPackagesMetadataIndexer extends DebianPackagesMetadataIndexerBase {

    private final Set<String> forcedArchitectures = Collections.emptySet();

    public DebianPackagesMetadataIndexer(Repository repo, RepositoryPathResolver resolver, ArtifactManagementService artifactManagementService) {
        super(repo, resolver, artifactManagementService);
    }

    public void indexPackages(List<DebianMetadata> mds, DebianPackagesContext packagesContext) {
        String coordinates = DebianUtils.print(packagesContext);
        log.debug("Writing Packages index for coordinates {}", coordinates);
        mds = DebianUtils.uniqueSortedMetadataList(mds);
        if (!DebianUtils.shouldDeletePackagesOfContext(mds, packagesContext, this.forcedArchitectures)) {
            File tmpFile = null;
            try {
                log.debug("Indexing coordinates: {}", coordinates);
                tmpFile = this.writePackagesIndex(mds, coordinates);
                this.writePackagesFileContentToRepo(packagesContext, tmpFile);
            } catch (CompressorException | IOException e) {
                throw new RuntimeException(e);
            } finally {
                DebianUtils.deleteTempFile(tmpFile);
            }
        } else {
            log.debug("Index for {} will be - no Debian packages exist for it and it's not a forced architecture.", coordinates);
        }
    }

    private File writePackagesIndex(Iterable<DebianMetadata> metadatas, String coordinates) throws IOException {
        File tempPackagesFile = this.getTempPackagesFilePath("134");
        try (Writer writer = new BufferedWriter(new FileWriter(tempPackagesFile), 8192)) {
            for (DebianMetadata metadata : metadatas) {
                this.writeMetadataEntry(writer, metadata);
            }
            writer.flush();
        }
        log.info("Writing Packages index of coordinates {} to fs temp took", coordinates);
        return tempPackagesFile;
    }

    private void writeMetadataEntry(Writer writer, DebianMetadata metadata) throws IOException {
        log.trace("Writing Debian Packages index entry for artifact '{}'", metadata.artifactRelativePath);
        writer.write(metadata.controlFileContent.trim() + "\n");
        writer.flush();
    }


}
