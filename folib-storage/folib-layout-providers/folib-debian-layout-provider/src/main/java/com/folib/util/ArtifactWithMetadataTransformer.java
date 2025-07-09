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

import com.folib.cache.DebianPackagesMetadataCache;
import com.folib.constant.DebianConstant;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactWithMetadata;
import com.folib.domain.DebianMetadata;
import com.folib.indexer.DebianIncrementalIndexer;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.lang.StringUtils;

import java.io.InputStream;
import java.util.function.Function;

/**
 * @author veadan
 * @since 2024-09-03 17:27
 */
@Slf4j
public class ArtifactWithMetadataTransformer implements Function<Artifact, ArtifactWithMetadata> {

    public static final String FAIL_REASON = "deb.index.status";
    private final Repository repo;
    private final DebianPackagesMetadataCache cache;

    private final DebianIncrementalIndexer debianIncrementalIndexer;

    public ArtifactWithMetadataTransformer(Repository repo, DebianPackagesMetadataCache cache,DebianIncrementalIndexer debianIncrementalIndexer) {
        this.repo = repo;
        this.cache = cache;
        this.debianIncrementalIndexer=debianIncrementalIndexer;
    }

    @Override
    public ArtifactWithMetadata apply(Artifact artifact) {
        DebianMetadata metadata = null;
        ArtifactWithMetadata artifactWithMetadata = null;
        if (this.cache != null) {
            metadata = this.cache.get(artifact, this.repo);
        }
        if (metadata == null) {
            metadata = this.extractAndCacheMetadata(artifact);
        }
        if (metadata != null && StringUtils.isNotBlank(metadata.packageName)) {
            artifactWithMetadata = new ArtifactWithMetadata(artifact, metadata);
        } else {
            log.error("Failed to retrieve metadata from artifact: {}", artifact.getArtifactPath());
        }
        return artifactWithMetadata;
    }

    private DebianMetadata extractAndCacheMetadata(Artifact artifact) {
        DebianMetadata metadata = null;
        try (InputStream is=debianIncrementalIndexer.getPackageByRepo(this.repo,artifact.getArtifactPath())){
            metadata = DebianUtils.extract(is);
            if (this.cache != null && metadata != null) {
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_FILENAME,metadata.getFilename(),artifact.getArtifactPath());
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_SIZE,null,String.valueOf(artifact.getSizeInBytes()));
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_MD5SUM,metadata.getMd5sum(),artifact.getChecksums().get(MessageDigestAlgorithms.MD5));
                DebianUtils.appendOrReplace(metadata, DebianConstant.CONTROL_SHA256,metadata.getSha256(),artifact.getChecksums().get(MessageDigestAlgorithms.SHA_256));
                this.cache.put(artifact, metadata);
            }
        } catch (Exception e) {
            String err = "Failed to extract and cache metadata for artifact " + artifact.getArtifactPath() + ": ";
            log.error(err + e.getMessage());
            log.debug(err, e);
        }
        return metadata;
    }
}
