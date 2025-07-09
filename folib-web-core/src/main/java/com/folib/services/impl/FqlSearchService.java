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
package com.folib.services.impl;

import com.alibaba.fastjson.JSON;
import com.google.common.collect.Lists;
import com.folib.artifact.coordinates.DockerCoordinates;
import com.folib.components.common.CommonComponent;
import com.folib.components.repository.RepositoryComponent;
import com.folib.data.criteria.Selector;
import com.folib.dependency.snippet.SnippetGenerator;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactEntity;
import com.folib.enums.SafeLevelEnum;
import com.folib.gremlin.adapters.ArtifactAdapter;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.repositories.ArtifactRepository;
import com.folib.schema2.ImageManifest;
import com.folib.schema2.LayerManifest;
import com.folib.aql.services.AqlSearchService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.DirectoryListingService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.search.SearchResult;
import com.folib.storage.search.SearchResults;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.time.DateFormatUtils;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.time.ZoneId;
import java.util.*;

@Slf4j
@Component
@Transactional
public class FqlSearchService extends GremlinVertexRepository<Artifact> implements AqlSearchService {
    @Inject
    ArtifactAdapter artifactAdapter;

    @Inject
    ArtifactRepository artifactRepository;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;
    @Inject
    private SnippetGenerator snippetGenerator;

    @Inject
    @Qualifier("browseRepositoryDirectoryListingService")
    private volatile DirectoryListingService directoryListingService;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private CommonComponent commonComponent;

    @Inject
    @Lazy
    private RepositoryComponent repositoryComponent;

    @Override
    public SearchResults search(Selector<ArtifactEntity> selector) throws IOException {

        return null;
    }

    @Override
    protected EntityTraversalAdapter<Vertex, Artifact> adapter() {
        return artifactAdapter;
    }


    public SearchResults artifactQuery(Boolean regex, String artifactName,
                                       String metadataSearch,
                                       String storageId,
                                       String repositoryId,
                                       String beginDate,
                                       String endDate,
                                       String sortField,
                                       String sortOrder,
                                       List<String> repositoryIds,
                                       String safeLevel,
                                       String digestAlgorithm,
                                       String digest,
                                       String query,
                                       Integer limit, Integer page) throws IOException {

        Pageable pageable = null;
        if (Objects.isNull(page)) {
            page = 1;
        }
        if (Objects.isNull(limit)) {
            limit = 5;
        }
        if (page == 1) {
            pageable = PageRequest.of(page, limit).first();
        } else {
            pageable = PageRequest.of(page, limit).previous();
        }
        List<String> storageIdAndRepositoryIdList = null;
        if (!commonComponent.hasAdmin()) {
            storageIdAndRepositoryIdList = repositoryComponent.getStorageIdAndRepositoryIdList();
            if (CollectionUtils.isEmpty(storageIdAndRepositoryIdList)) {
                SearchResults result = new SearchResults();
                result.setTotal(0);
                result.setResults(Collections.emptySet());
                return result;
            }
        }
        if (SafeLevelEnum.SCAN_COMPLETE.getLevel().equalsIgnoreCase(safeLevel)) {
            storageIdAndRepositoryIdList = null;
        }
        Page<Artifact> artifacts = artifactRepository.findMatchingByIndex(pageable, regex, artifactName, metadataSearch, storageId, repositoryId, repositoryIds, storageIdAndRepositoryIdList, beginDate, endDate, safeLevel, digestAlgorithm, digest, query, sortField, sortOrder);
        List<Artifact> artifactEntityList = artifacts.getContent();

        SearchResults result = new SearchResults();
        result.setTotal(artifacts.getTotalElements());
        String df = "yyyy-MM-dd HH:mm:ss";
        for (Artifact artifact : artifactEntityList) {
            SearchResult r = new SearchResult();
            result.getResults().add(r);
            r.setStorageId(artifact.getStorageId());
            r.setRepositoryId(artifact.getRepositoryId());
            r.setArtifactCoordinates(artifact.getArtifactCoordinates());

            RepositoryPath repositoryPath = repositoryPathResolver.resolve(artifact.getStorageId(),
                    artifact.getRepositoryId(),
                    artifact.getArtifactPath());

            r.setChecksums(artifact.getChecksums());
            r.setSizeInBytes(artifact.getSizeInBytes());
            r.setDownloadCount(artifact.getDownloadCount());
            r.setVulnerabilitiesCount(artifact.getVulnerabilitiesCount());
            r.setCriticalVulnerabilitiesCount(artifact.getCriticalVulnerabilitiesCount());
            r.setHighVulnerabilitiesCount(artifact.getHighVulnerabilitiesCount());
            r.setMediumVulnerabilitiesCount(artifact.getMediumVulnerabilitiesCount());
            r.setLowVulnerabilitiesCount(artifact.getLowVulnerabilitiesCount());
            r.setSuppressedVulnerabilitiesCount(artifact.getSuppressedVulnerabilitiesCount());
            String createdTime = DateFormatUtils.format(Date.from(artifact.getCreated().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
            r.setCreated(createdTime);
            if (Objects.nonNull(artifact.getLastUsed())) {
                String lastUpdatedTime = DateFormatUtils.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                String lastUsedTime = DateFormatUtils.format(Date.from(artifact.getLastUsed().atZone(ZoneId.of("Asia/Shanghai")).toOffsetDateTime().toInstant()), df);
                r.setLastUpdated(lastUpdatedTime);
                r.setLastUsed(lastUsedTime);
            }
            r.setSha(artifact.getChecksums().get("SHA-1"));
            r.setMd5(artifact.getChecksums().get("MD5"));

            //生成snippets
            Repository repository = repositoryPath.getRepository();
            URL artifactResource = RepositoryFiles.readResourceUrl(repositoryPath);
            r.setUrl(artifactResource.toString());
            if (Objects.nonNull(repository)) {
                r.setLayout(repository.getLayout());
                r.setSubLayout(repository.getSubLayout());
            }
            String path = artifact.getArtifactCoordinates().buildPath();
            if (DockerLayoutProvider.ALIAS.equalsIgnoreCase(r.getLayout())) {
                //docker
                DockerCoordinates dockerArtifactCoordinates = (DockerCoordinates) artifact.getArtifactCoordinates();
                r.setArtifactName(dockerArtifactCoordinates.getTAG());
                r.setArtifactPath(dockerArtifactCoordinates.getIMAGE_NAME().replace(":", "/"));
                String blobs = "blobs";
                String manifest = "manifest";
                String artifactPath = repositoryPath.toAbsolutePath().toString();
                if (artifactPath.contains("sha256") && !artifactPath.contains(blobs) && !artifactPath.contains(manifest) && DockerCoordinates.include(artifactPath)) {
                    r.setSizeInBytes(getSearchDockerSize(repositoryPath, r.getArtifactPath().replace("/" + r.getArtifactName(), "")));
                }
            } else {
                r.setArtifactName(path.substring(path.lastIndexOf("/") + 1));
                r.setArtifactPath(path);
            }
        }
        return result;
    }

    private Long getSearchDockerSize(RepositoryPath repositoryPath, String imageName) {
        Long size = 0L;
        try {
            if (Objects.isNull(repositoryPath) || !Files.exists(repositoryPath)) {
                return size;
            }
            String manifestString = Files.readString(repositoryPath);
            ImageManifest imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
            List<LayerManifest> layers = null;
            if (CollectionUtils.isNotEmpty(imageManifest.getLayers())) {
                layers = imageManifest.getLayers();
            } else if (CollectionUtils.isNotEmpty(imageManifest.getManifests())) {
                RepositoryPath manifestPath = repositoryPathResolver.resolve(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(), "manifest/" + imageManifest.getManifests().get(0).getDigest());
                if (Objects.isNull(manifestPath) || !Files.exists(manifestPath)) {
                    return size;
                }
                manifestString = Files.readString(manifestPath);
                imageManifest = JSON.parseObject(manifestString, ImageManifest.class);
                layers = imageManifest.getLayers();
            }
            if (CollectionUtils.isNotEmpty(layers)) {
                size = layers.stream().filter(item -> Objects.nonNull(item.getSize())).mapToLong(LayerManifest::getSize).sum();
            }
        } catch (Exception ex) {
            log.warn("计算Docker镜像大小错误：镜像 [{}] [{}] 错误信息 [{}]", repositoryPath.toString(), imageName, ExceptionUtils.getStackTrace(ex));
        }
        return size;
    }

    private List<String> getAllRepository() {
        final List<Storage> storageList = new ArrayList<>(configurationManagementService.getConfiguration()
                .getStorages()
                .values());
        List<String> allStorageIdAndRepositoryIdList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(storageList)) {
            for (Storage storage : storageList) {
                if (MapUtils.isNotEmpty(storage.getRepositories())) {
                    for (Repository repository : storage.getRepositories().values()) {
                        allStorageIdAndRepositoryIdList.add(String.format("%s-%s", storage.getId(), repository.getId()));
                    }
                }
            }
        }
        return allStorageIdAndRepositoryIdList;
    }
}
