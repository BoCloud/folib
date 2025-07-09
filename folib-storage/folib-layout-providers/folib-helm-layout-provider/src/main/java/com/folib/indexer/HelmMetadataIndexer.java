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


import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.annotations.VisibleForTesting;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import com.google.common.collect.Sets;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.*;
import java.util.stream.Stream;

import com.folib.model.HelmChartMetadata;
import com.folib.model.HelmDependencyMetadata;
import com.folib.model.HelmIndexYamlMetadata;
import com.folib.model.HelmMetadata;
import com.folib.providers.io.RepositoryFiles;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.io.RootRepositoryPath;
import com.folib.services.ArtifactManagementService;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.util.CollectionHelmUtils;
import com.folib.util.HelmUtils;
import lombok.Generated;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StopWatch;

public class HelmMetadataIndexer {
    @Generated
    private static final Logger log = LoggerFactory.getLogger(HelmMetadataIndexer.class);
    //private final String baseUrl;
    private final HelmMetadataExtractor helmMetadataExtractor;
    protected ArtifactManagementService artifactManagementService;
    protected RepositoryPathResolver repositoryPathResolver;
    private final ObjectMapper yamlObjectMapper;
    private String storageId;
    private String repositoryId;


    public HelmMetadataIndexer(String storageId, String repositoryId, ArtifactManagementService artifactManagementService, RepositoryPathResolver repositoryPathResolver) {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
        //this.baseUrl = baseUrl;

        this.helmMetadataExtractor = new HelmMetadataExtractor();
        this.artifactManagementService = artifactManagementService;
        this.repositoryPathResolver = repositoryPathResolver;
        this.yamlObjectMapper = HelmUtils.getYamlObjectMapper();
    }


    public void reindexAsSystem() {
        log.info("Starting reindexing Helm local repository: '{}'.", this.repositoryId);
        long startedTime = System.currentTimeMillis();

        HelmIndexYamlMetadata indexYaml = HelmUtils.emptyIndexYaml();
        indexYaml.entries = Maps.newConcurrentMap();
        reindex(indexYaml);
        writeToIndexYaml(indexYaml);
        log.info("Finished reindexing Helm local repository: {}, including writing to index.yaml. Took {} ms", this.repositoryId,
                Long.valueOf(System.currentTimeMillis() - startedTime));
    }


    private void addIndex(String path, HelmIndexYamlMetadata helmIndexYamlMetadata) {
        try {
            RepositoryPath repositoryPath = this.repositoryPathResolver.resolve(this.storageId, this.repositoryId, path);
            HelmMetadata metadata = this.helmMetadataExtractor.extract(repositoryPath);
            StopWatch timer = new StopWatch();
            log.debug("Indexing Helm metadata for repo '{}' on path '{}'", this.repositoryId, path);
            if (metadata.helmChart != null) {
                writeAttributes(path, metadata);
                log.debug("Updating index.yaml for repository {}", this.repositoryId);
                updateIndexYaml(metadata, repositoryPath, helmIndexYamlMetadata);
            }
            timer.stop();
            log.trace("Indexing Helm metadata for repo '{}' on path '{}' took '{}'", this.repositoryId, path, timer);
        } catch (Exception e) {
            log.error("Error occurred while indexing: {}", e.getMessage());
            log.debug("Error occurred while indexing:", e);
        }
    }

    private void reindex(HelmIndexYamlMetadata helmIndexYamlMetadata) {
        long startedTime = System.currentTimeMillis();
        RootRepositoryPath repositoryPath = this.repositoryPathResolver.resolve(this.storageId, this.repositoryId);
        try {
            try (Stream<Path> pathStream = Files.list(repositoryPath.getTarget()))
            {
                pathStream.filter(path -> {
                    if (path.getFileName().toString().endsWith(".tgz")) {
                        return true;
                    }
                    return false;
                }).forEach(path -> {
                    addIndex(path.getFileName().toString(), helmIndexYamlMetadata);
                });
            }
        } catch (Exception e) {
            log.error("Error occurred while indexing: {}", e.getMessage());
            throw new RuntimeException(e);
        }
        log.debug("Finished reindexing Helm local repository: {}, without writing to index.yaml. Took {} ms", this.repositoryId,

                Long.valueOf(System.currentTimeMillis() - startedTime));
    }

    private void writeAttributes(String path, HelmMetadata metadata) {
        HelmChartMetadata chart = metadata.helmChart;
        List<HelmDependencyMetadata> dependencies = metadata.dependencies;
        HashMultimap hashMultimap = HashMultimap.create();
        log.debug("Writing metadata attributes on chart {}:{} path: {}", chart.name, chart.version, path);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.name", chart.name);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.version", chart.version);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.appVersion", chart.appVersion);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.description", chart.description);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.home", chart.home);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.created", chart.created);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.type", chart.type);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.annotations", chart.annotations);
        CollectionHelmUtils.addNotNullAndBlankPropertyToMultimap(hashMultimap, "chart.apiVersion", chart.apiVersion);
        if (chart.deprecated != null && chart.deprecated.booleanValue()) {
            hashMultimap.put("chart.isDeprecated", "true");
        }
        if (CollectionHelmUtils.isNotNullOrEmpty(chart.sources)) {
            CollectionHelmUtils.addNotNullMultiValuePropertyToMultimap((Multimap) hashMultimap, "chart.sources", chart.sources.toArray());
        }
        if (CollectionHelmUtils.isNotNullOrEmpty(chart.maintainers)) {
            CollectionHelmUtils.addNotNullMultiValuePropertyToMultimap((Multimap) hashMultimap, "chart.maintainers", chart.maintainers.toArray());
        }
        if (CollectionHelmUtils.isNotNullOrEmpty(dependencies)) {
            CollectionHelmUtils.addNotNullMultiValuePropertyToMultimap((Multimap) hashMultimap, "chart.dependencies", dependencies.toArray());
        }
        //this.repositoryService.setAttributes(this.repoKey, path, (Multimap)hashMultimap);
    }

    private void updateIndexYaml(HelmMetadata metadata, RepositoryPath repositoryPath, HelmIndexYamlMetadata indexYaml) throws IOException {
        String chartName = metadata.helmChart.name;
        if (StringUtils.isBlank(chartName) || StringUtils.isBlank(metadata.helmChart.version)) {
            log.error("Cannot add Chart {} to index.yaml - Chart name or version is empty!", repositoryPath);
            return;
        }
        fillIndexChartMetadata(metadata, repositoryPath);
        if (isValidChart(metadata)) {
            log.trace("Adding the chart:{}, version:{} into the index.yaml", chartName, metadata.helmChart.version);
            indexYaml.entries.compute(chartName, (key, charts) -> {
                if (charts == null) {
                    charts = new TreeSet();
                }
                charts.remove(metadata.helmChart);
                charts.add(metadata.helmChart);
                return charts;
            });
        }
    }

    @VisibleForTesting
    boolean isValidChart(HelmMetadata metadata) {
        log.trace("Validating chart:{},version{} before updating the index.yaml", metadata.helmChart.name, metadata.helmChart.version);
        String chartName = metadata.helmChart.name;
        HelmIndexYamlMetadata helmIndexYamlMetadata = new HelmIndexYamlMetadata();
        helmIndexYamlMetadata.entries = Maps.newConcurrentMap();
        helmIndexYamlMetadata.entries.computeIfAbsent(chartName, k -> Sets.newTreeSet());
        ((SortedSet<HelmChartMetadata>) helmIndexYamlMetadata.entries.get(chartName)).add(metadata.helmChart);
        try {
            InputStream stream = HelmUtils.indexYamlToInputStream(helmIndexYamlMetadata);
            this.yamlObjectMapper.readValue(stream, HelmIndexYamlMetadata.class);
        } catch (Exception e) {
            log.warn("Chart '{}' is not valid due to:{}, and will be omitted from the index.yaml", chartName, e
                    .getMessage());
            log.debug("Chart '{}' is not valid due to:", chartName, e);
            return false;
        }
        return true;
    }

    private void removeIndexEntry(String name, String version, HelmIndexYamlMetadata indexYaml) {
        if (indexYaml.entries != null) {
            Set<HelmChartMetadata> helmChartMetadata = (Set<HelmChartMetadata>) indexYaml.entries.get(name);
            if (helmChartMetadata != null) {
                helmChartMetadata.removeIf(chart -> chart.version.equals(version));
                if (((SortedSet<?>) indexYaml.entries.get(name)).isEmpty()) {
                    indexYaml.entries.remove(name);
                }
            }
        }
    }

    private void fillIndexChartMetadata(HelmMetadata metadata, RepositoryPath repositoryPath) throws IOException {
        if (Objects.nonNull(repositoryPath)) {
            Path filePath = Paths.get(String.join(".", repositoryPath.getTarget().toString(), "sha256"));
            metadata.helmChart.digest = readSHA256FileContent(filePath);
            if (!RepositoryTypeEnum.PROXY.getType().equals(repositoryPath.getRepository().getType())) {
                metadata.helmChart.urls = Lists.newArrayList(RepositoryFiles.relativizePath(repositoryPath));
            }
        }
        metadata.helmChart.created = Instant.now().toString();
    }

    private HelmIndexYamlMetadata readFromIndexYaml() {
        try {
            HelmIndexYamlMetadata indexYaml;
            RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "index.yaml");
            if (Files.exists(repositoryPath)) {
                InputStream indexStream = Files.newInputStream(repositoryPath);
                try {
                    indexYaml = (HelmIndexYamlMetadata) this.yamlObjectMapper.readValue(indexStream, HelmIndexYamlMetadata.class);
                    indexStream.close();
                } catch (Throwable throwable) {
                    try {
                        indexStream.close();
                    } catch (Throwable throwable1) {
                        throwable.addSuppressed(throwable1);
                    }
                    throw throwable;
                }
            } else {
                indexYaml = HelmUtils.emptyIndexYaml();
            }
            if (indexYaml.entries == null) {
                indexYaml.entries = Maps.newConcurrentMap();
            }
            return indexYaml;
        } catch (Exception e) {
            log.error("Metadata indexing failing on helm repository {} : {}", this.repositoryId, e.getMessage());
            log.debug("Metadata indexing failing on helm repository " + this.repositoryId + ":", e);
            return null;
        }
    }

    public HelmIndexYamlMetadata readFromIndexYaml(RepositoryPath repositoryPath) {
        try {
            HelmIndexYamlMetadata indexYaml;
            if (Files.exists(repositoryPath)) {
                InputStream indexStream = Files.newInputStream(repositoryPath);
                try {
                    indexYaml = (HelmIndexYamlMetadata) this.yamlObjectMapper.readValue(indexStream, HelmIndexYamlMetadata.class);
                    indexStream.close();
                } catch (Throwable throwable) {
                    try {
                        indexStream.close();
                    } catch (Throwable throwable1) {
                        throwable.addSuppressed(throwable1);
                    }
                    throw throwable;
                }
            } else {
                indexYaml = HelmUtils.emptyIndexYaml();
            }
            if (indexYaml.entries == null) {
                indexYaml.entries = Maps.newConcurrentMap();
            }
            return indexYaml;
        } catch (Exception e) {
            log.error("Metadata indexing failing on helm repository {} : {}", this.repositoryId, e.getMessage());
            log.debug("Metadata indexing failing on helm repository " + this.repositoryId + ":", e);
            return null;
        }
    }

    private void writeToIndexYaml(HelmIndexYamlMetadata indexYaml) {
        try {

            try (InputStream inputStream = HelmUtils.indexYamlToInputStream(indexYaml)){
                RepositoryPath repositoryPath = repositoryPathResolver.resolve(storageId, repositoryId, "index.yaml");
                artifactManagementService.store(repositoryPath, inputStream);
                inputStream.close();
            } catch (Throwable throwable) {
                throw throwable;
            }
        } catch (IOException e) {
            log.error("Metadata indexing failing on helm repository {} : {} ", this.repositoryId, e.getMessage());
            log.debug("Metadata indexing failing on helm repository " + this.repositoryId + ":", e);
        }
    }

    public static String readSHA256FileContent(Path filePath) throws IOException {
        try {
            return new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw e;
        }
    }
}
