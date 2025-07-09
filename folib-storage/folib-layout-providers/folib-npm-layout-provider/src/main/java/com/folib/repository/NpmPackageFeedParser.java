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
package com.folib.repository;

import com.alibaba.fastjson.JSONObject;
import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.NpmCoordinates;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.domain.ArtifactTagEntity;
import com.folib.enums.NpmPacketSuffix;
import com.folib.enums.NpmSubLayout;
import com.folib.npm.metadata.*;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.repository.Repository;
import com.folib.util.LocalDateTimeInstance;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Slf4j
@Component
public class NpmPackageFeedParser {
    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupService repositoryArtifactIdGroupService;

    @Inject
    private ArtifactRepository artifactRepository;

    public void parseSearchResult(Repository repository,
                                  SearchResults searchResults) {
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();

        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (SearchResult searchResult : searchResults.getObjects()) {
            PackageEntry packageEntry = searchResult.getPackage();

            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, packageEntry,packageSuffix);
            if (remoteArtifactEntry == null) {
                continue;
            }

            remoteArtifactEntry.getTagSet().add(lastVersionTag);

            artifactToSaveSet.add(remoteArtifactEntry);
        }

        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);

    }

    public void parseFeed(Repository repository,
                          PackageFeed packageFeed) {
        if (packageFeed == null) {
            return;
        }

        String repositoryId = repository.getId();
        String storageId = repository.getStorage().getId();

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
        final String packageSuffix = NpmSubLayout.OHPM.getValue().equals(repository.getSubLayout()) ? NpmPacketSuffix.HAR.getValue() : NpmPacketSuffix.TGZ.getValue();
        Versions versions = packageFeed.getVersions();
        if (versions == null) {
            return;
        }

        Map<String, PackageVersion> versionMap = versions.getAdditionalProperties();
        if (versionMap == null || versionMap.isEmpty()) {
            return;
        }

        Set<Artifact> artifactToSaveSet = new HashSet<>();
        for (PackageVersion packageVersion : versionMap.values()) {
            log.info("storageId [{}] repositoryId [{}] name [{}] packageVersion [{}]", storageId, repositoryId, packageFeed.getName(), packageVersion);
            ArtifactEntity remoteArtifactEntry = parseVersion(storageId, repositoryId, packageVersion,packageSuffix);
            if (remoteArtifactEntry == null) {
                continue;
            }

            if (packageVersion.getVersion().equals(packageFeed.getDistTags().getLatest())) {
                remoteArtifactEntry.getTagSet().add(lastVersionTag);
            }

            artifactToSaveSet.add(remoteArtifactEntry);
        }
        repositoryArtifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PackageVersion packageVersion,String packageSuffix) {

        NpmCoordinates c = NpmCoordinates.of(packageVersion.getName(), packageVersion.getVersion(),packageSuffix);
        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
        ArtifactEntity remoteArtifactEntry = null;
        if (Objects.nonNull(artifact)) {
            //已存在
            remoteArtifactEntry = new ArtifactEntity(artifact.getNativeId(), storageId, repositoryId, artifact.getUuid(), c);
        } else {
            //不存在
            remoteArtifactEntry = new ArtifactEntity(storageId, repositoryId, c);
            remoteArtifactEntry.setStorageId(storageId);
            remoteArtifactEntry.setRepositoryId(repositoryId);
            remoteArtifactEntry.setArtifactCoordinates(c);
            remoteArtifactEntry.setDownloadCount(0);
            remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);
        }
        Dependency dependency = packageVersion.getDependencies();
        if (Objects.nonNull(dependency) && MapUtils.isNotEmpty(dependency.getAdditionalProperties())) {
            remoteArtifactEntry.setDependencies(JSONObject.toJSONString(packageVersion.getDependencies().getAdditionalProperties()));
        }
        //make HEAD request for `tarball` URL ???
        // remoteArtifactEntry.setSizeInBytes(packageVersion.getProperties().getPackageSize());
        return remoteArtifactEntry;
    }

    private ArtifactEntity parseVersion(String storageId,
                                        String repositoryId,
                                        PackageEntry packageEntry,String packageSuffix) {
        String scope = packageEntry.getScope();
        String packageId = NpmCoordinates.calculatePackageId("unscoped".equals(scope) ? null : scope,
                packageEntry.getName());

        NpmCoordinates c = NpmCoordinates.of(packageId, packageEntry.getVersion(),packageSuffix);

        LocalDateTime now = LocalDateTimeInstance.now();
        Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
        ArtifactEntity remoteArtifactEntry = null;
        if (Objects.nonNull(artifact)) {
            //已存在
            remoteArtifactEntry = new ArtifactEntity(artifact.getNativeId(), storageId, repositoryId, artifact.getUuid(), c);
        } else {
            //不存在
            remoteArtifactEntry.setStorageId(storageId);
            remoteArtifactEntry.setRepositoryId(repositoryId);
            remoteArtifactEntry.setArtifactCoordinates(c);
            remoteArtifactEntry.setDownloadCount(0);
            remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);
        }
        return remoteArtifactEntry;
    }

}
