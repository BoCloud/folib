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
package com.folib.gremlin.adapters;

import com.alibaba.fastjson.JSON;
import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.domain.*;
import com.folib.artifact.ArtifactTag;
import com.folib.configuration.ConfigurationManager;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.EntityTraversalUtils;
import com.folib.gremlin.dsl.__;
import com.folib.storage.repository.Repository;
import com.folib.strategy.ArtifactStrategyFactory;
import com.folib.util.UserUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.stream.Collectors;

import static cn.hutool.core.convert.Convert.toLocalDateTime;
import static com.folib.gremlin.dsl.EntityTraversalUtils.*;
import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;

/**
 * @author veadan
 */
@Slf4j
@Component
public class ArtifactAdapter implements VertexEntityTraversalAdapter<Artifact> {

    @Inject
    ArtifactCoordinatesHierarchyAdapter artifactCoordinatesAdapter;
    @Inject
    ArtifactTagAdapter artifactTagAdapter;
    @Inject
    VulnerabilityAdapter vulnerabilityAdapter;
    @Inject
    ComponentAdapter componentAdapter;
    @Inject
    @Lazy
    ConfigurationManager configurationManager;
    @Inject
    @Lazy
    ArtifactStrategyFactory strategyFactory;

    @Override
    public String label() {
        return Vertices.ARTIFACT;
    }

    @Override
    public EntityTraversal<Vertex, Artifact> fold() {
        return fold(Optional.of(GenericCoordinatesEntity.class));
    }

    public EntityTraversal<Vertex, Artifact> fold(Optional<Class<? extends GenericCoordinates>> layoutArtifactCoordinatesClass) {
        return __.<Vertex, Object>project("id",
                "uuid",
                "storageId",
                "repositoryId",
                "storageIdAndRepositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "evidenceQuantity",
                "vulnerabilities",
                "dependencyCount",
                "dependencyVulnerabilitiesCount",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "metadata",
                "scanDate",
                "scanDateTime",
                "dependencies",
                "filePaths",
                "filenames",
                "checksums",
                "artifactCoordinates",
                "tags",
                "vulnerabilitySet",
                "artifactFileExists",
                "promotion",
                "promotionNodes",
                "enabled",
                "createdBy",
                "updatedBy",
                "artifactName",
                "artifactPath",
                "packageInfo",
                "layers",
                "componentSet")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("storageIdAndRepositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValue("evidenceQuantity"))
                .by(__.enrichPropertyValues("vulnerabilities"))
                .by(__.enrichPropertyValue("dependencyCount"))
                .by(__.enrichPropertyValue("dependencyVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("metadata"))
                .by(__.enrichPropertyValue("scanDate"))
                .by(__.enrichPropertyValue("scanDateTime"))
                .by(__.enrichPropertyValue("dependencies"))
                .by(__.enrichPropertyValues("filePaths"))
                .by(__.enrichPropertyValues("filenames"))
                .by(__.enrichPropertyValues("checksums"))
                .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                        .mapToObject(__.inV()
                                .map(artifactCoordinatesAdapter.fold(layoutArtifactCoordinatesClass))
                                .map(EntityTraversalUtils::castToObject)))
                .by(__.outE(Edges.ARTIFACT_HAS_TAGS)
                        .inV()
                        .map(artifactTagAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .inV()
                        .map(vulnerabilityAdapter.baseFold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("artifactFileExists"))
                .by(__.enrichPropertyValue("promotion"))
                .by(__.enrichPropertyValues("promotionNodes"))
                .by(__.enrichPropertyValue("enabled"))
                .by(__.enrichPropertyValue("createdBy"))
                .by(__.enrichPropertyValue("updatedBy"))
                .by(__.enrichPropertyValue("artifactName"))
                .by(__.enrichPropertyValue("artifactPath"))
                .by(__.enrichPropertyValue("packageInfo"))
                .by(__.enrichPropertyValues("layers"))
                .by(__.outE(Edges.ARTIFACT_HAS_COMPONENTS)
                        .inV()
                        .map(componentAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .map(this::map);
    }

    public EntityTraversal<Vertex, Artifact> reportFold(Optional<Class<? extends GenericCoordinates>> layoutArtifactCoordinatesClass) {
        return __.<Vertex, Object>project("id",
                "uuid",
                "storageId",
                "repositoryId",
                "storageIdAndRepositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "evidenceQuantity",
                "vulnerabilities",
                "dependencyCount",
                "dependencyVulnerabilitiesCount",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "metadata",
                "report",
                "scanDate",
                "scanDateTime",
                "dependencies",
                "filePaths",
                "filenames",
                "checksums",
                "artifactCoordinates",
                "tags",
                "vulnerabilitySet",
                "artifactFileExists",
                "promotion",
                "promotionNodes",
                "enabled",
                "createdBy",
                "updatedBy",
                "artifactName",
                "artifactPath",
                "packageInfo",
                "componentSet")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("storageIdAndRepositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValue("evidenceQuantity"))
                .by(__.enrichPropertyValues("vulnerabilities"))
                .by(__.enrichPropertyValue("dependencyCount"))
                .by(__.enrichPropertyValue("dependencyVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("metadata"))
                .by(__.enrichPropertyValue("report"))
                .by(__.enrichPropertyValue("scanDate"))
                .by(__.enrichPropertyValue("scanDateTime"))
                .by(__.enrichPropertyValue("dependencies"))
                .by(__.enrichPropertyValues("filePaths"))
                .by(__.enrichPropertyValues("filenames"))
                .by(__.enrichPropertyValues("checksums"))
                .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                        .mapToObject(__.inV()
                                .map(artifactCoordinatesAdapter.fold(layoutArtifactCoordinatesClass))
                                .map(EntityTraversalUtils::castToObject)))
                .by(__.outE(Edges.ARTIFACT_HAS_TAGS)
                        .inV()
                        .map(artifactTagAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .inV()
                        .map(vulnerabilityAdapter.baseFold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("artifactFileExists"))
                .by(__.enrichPropertyValue("promotion"))
                .by(__.enrichPropertyValues("promotionNodes"))
                .by(__.enrichPropertyValue("enabled"))
                .by(__.enrichPropertyValue("createdBy"))
                .by(__.enrichPropertyValue("updatedBy"))
                .by(__.enrichPropertyValue("artifactName"))
                .by(__.enrichPropertyValue("artifactPath"))
                .by(__.enrichPropertyValue("packageInfo"))
                .by(__.outE(Edges.ARTIFACT_HAS_COMPONENTS)
                        .inV()
                        .map(componentAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .map(this::map);
    }

    public EntityTraversal<Vertex, Artifact> baseFold(Optional<Class<? extends GenericCoordinates>> layoutArtifactCoordinatesClass) {
        return __.<Vertex, Object>project("id",
                "uuid",
                "storageId",
                "repositoryId",
                "storageIdAndRepositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "filePaths",
                "artifactCoordinates",
                "tags",
                "artifactFileExists",
                "enabled",
                "createdBy",
                "updatedBy",
                "artifactName",
                "artifactPath",
                "scanDateTime",
                "metadata")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("storageIdAndRepositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValues("filePaths"))
                .by(__.mapToObject(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES).inV()
                                .map(artifactCoordinatesAdapter.fold(layoutArtifactCoordinatesClass))
                                .map(EntityTraversalUtils::castToObject)
                        ))
                .by(__.out(Edges.ARTIFACT_HAS_TAGS)
                        .map(artifactTagAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("artifactFileExists"))
                .by(__.enrichPropertyValue("enabled"))
                .by(__.enrichPropertyValue("createdBy"))
                .by(__.enrichPropertyValue("updatedBy"))
                .by(__.enrichPropertyValue("artifactName"))
                .by(__.enrichPropertyValue("artifactPath"))
                .by(__.enrichPropertyValue("scanDateTime"))
                .by(__.enrichPropertyValue("metadata"))
                .map(this::map);
    }

    public EntityTraversal<Vertex, Artifact> searchFold(Optional<Class<? extends GenericCoordinates>> layoutArtifactCoordinatesClass) {
        return __.<Vertex, Object>project("id",
                "uuid",
                "storageId",
                "repositoryId",
                "storageIdAndRepositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "checksums",
                "artifactCoordinates",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "artifactFileExists",
                "enabled",
                "createdBy",
                "updatedBy",
                "artifactName",
                "artifactPath")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("storageIdAndRepositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValues("checksums"))
                .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                        .mapToObject(__.inV()
                                .map(artifactCoordinatesAdapter.fold(layoutArtifactCoordinatesClass))
                                .map(EntityTraversalUtils::castToObject)))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("artifactFileExists"))
                .by(__.enrichPropertyValue("enabled"))
                .by(__.enrichPropertyValue("createdBy"))
                .by(__.enrichPropertyValue("updatedBy"))
                .by(__.enrichPropertyValue("artifactName"))
                .by(__.enrichPropertyValue("artifactPath"))
                .map(this::map);
    }

    public EntityTraversal<Vertex, Artifact> aqlSearchFold(Optional<Class<? extends GenericCoordinates>> layoutArtifactCoordinatesClass) {
        return __.<Vertex, Object>project("id",
                "uuid",
                "storageId",
                "repositoryId",
                "storageIdAndRepositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "checksums",
                "artifactCoordinates",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "metadata",
                "artifactFileExists",
                "enabled",
                "createdBy",
                "updatedBy",
                "artifactName",
                "artifactPath")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("storageIdAndRepositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValues("checksums"))
                .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                        .mapToObject(__.inV()
                                .map(artifactCoordinatesAdapter.fold(layoutArtifactCoordinatesClass))
                                .map(EntityTraversalUtils::castToObject)))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("metadata"))
                .by(__.enrichPropertyValue("artifactFileExists"))
                .by(__.enrichPropertyValue("enabled"))
                .by(__.enrichPropertyValue("createdBy"))
                .by(__.enrichPropertyValue("updatedBy"))
                .by(__.enrichPropertyValue("artifactName"))
                .by(__.enrichPropertyValue("artifactPath"))
                .map(this::map);
    }


    public EntityTraversal<Vertex, VulnerabilityArtifactDomain> vulnerabilityFold() {
        return __.<Vertex, Object>project("id", "vulnerabilityID",
                "uuid",
                "storageId",
                "repositoryId",
                "lastUpdated",
                "lastUsed",
                "created",
                "sizeInBytes",
                "downloadCount",
                "safeLevel",
                "evidenceQuantity",
                "vulnerabilities",
                "dependencyCount",
                "dependencyVulnerabilitiesCount",
                "vulnerabilitiesCount",
                "criticalVulnerabilitiesCount",
                "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount",
                "lowVulnerabilitiesCount",
                "suppressedVulnerabilitiesCount",
                "dependencies",
                "filenames",
                "checksums",
                "artifactCoordinates",
                "tags",
                "vulnerabilitySet",
                "artifactFileExists")
                .by(__.id())
                .by(__.select("v").values("uuid"))
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("lastUsed"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("sizeInBytes"))
                .by(__.enrichPropertyValue("downloadCount"))
                .by(__.enrichPropertyValue("safeLevel"))
                .by(__.enrichPropertyValue("evidenceQuantity"))
                .by(__.enrichPropertyValues("vulnerabilities"))
                .by(__.enrichPropertyValue("dependencyCount"))
                .by(__.enrichPropertyValue("dependencyVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("dependencies"))
                .by(__.enrichPropertyValues("filenames"))
                .by(__.enrichPropertyValues("checksums"))
                .by(__.outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                        .mapToObject(__.inV()
                                .map(artifactCoordinatesAdapter.fold(Optional.of(GenericCoordinatesEntity.class)))
                                .map(EntityTraversalUtils::castToObject)))
                .by(__.outE(Edges.ARTIFACT_HAS_TAGS)
                        .inV()
                        .map(artifactTagAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .inV()
                        .map(vulnerabilityAdapter.fold())
                        .map(EntityTraversalUtils::castToObject)
                        .fold())
                .by(__.enrichPropertyValue("artifactFileExists"))
                .map(this::vulnerabilityMap);
    }

    private VulnerabilityArtifactDomain vulnerabilityMap(Traverser<Map<String, Object>> t) {
        String storageId = extractObject(String.class, t.get().get("storageId"));
        String repositoryId = extractObject(String.class, t.get().get("repositoryId"));
        ArtifactCoordinates artifactCoordinates = null;
        if (t.get().get("artifactCoordinates") instanceof GenericCoordinatesEntity entity) {
            Repository repository = configurationManager.getRepository(storageId, repositoryId);
            if (repository == null) {
                throw new IllegalStateException("Repository not found: " + storageId + ":" + repositoryId);
            }
            Class<? extends GenericCoordinates> clazz = Optional.ofNullable(repository.getLayout())
                    .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                    .map(ArtifactLayoutDescription::getArtifactCoordinatesClass)
                    .orElseThrow(() -> new IllegalStateException("Coordinates class not found"));

            artifactCoordinates =   strategyFactory.getArtifactCoordinates(clazz, entity);
        } else {
            artifactCoordinates = extractObject(ArtifactCoordinates.class,
                    t.get().get("artifactCoordinates"));
        }
        VulnerabilityArtifactDomain result = new VulnerabilityArtifactDomain();
        result.setStorageId(storageId);
        result.setRepositoryId(repositoryId);
        result.setArtifactCoordinates(artifactCoordinates);
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));
        result.setVulnerabilityId(extractObject(String.class, t.get().get("vulnerabilityID")));
        result.setCreated(toLocalDateTime(extractObject(Long.class, t.get().get("created"))));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setLastUsed(toLocalDateTime(extractObject(Long.class, t.get().get("lastUsed"))));
        result.setSizeInBytes(extractObject(Long.class, t.get().get("sizeInBytes")));
        result.setDownloadCount(extractObject(Integer.class, t.get().get("downloadCount")));

        result.setSafeLevel(extractObject(String.class, t.get().get("safeLevel")));
        result.setEvidenceQuantity(extractObject(Integer.class, t.get().get("evidenceQuantity")));
        result.setVulnerabilities(extractPropertyList(String.class, t.get().get("vulnerabilities")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        result.setDependencyCount(extractObject(Integer.class, t.get().get("dependencyCount")));
        result.setDependencyVulnerabilitiesCount(extractObject(Integer.class, t.get().get("dependencyVulnerabilitiesCount")));
        result.setVulnerabilitiesCount(extractObject(Integer.class, t.get().get("vulnerabilitiesCount")));
        result.setCriticalVulnerabilitiesCount(extractObject(Integer.class, t.get().get("criticalVulnerabilitiesCount")));
        result.setHighVulnerabilitiesCount(extractObject(Integer.class, t.get().get("highVulnerabilitiesCount")));
        result.setMediumVulnerabilitiesCount(extractObject(Integer.class, t.get().get("mediumVulnerabilitiesCount")));
        result.setLowVulnerabilitiesCount(extractObject(Integer.class, t.get().get("lowVulnerabilitiesCount")));
        result.setSuppressedVulnerabilitiesCount(extractObject(Integer.class, t.get().get("suppressedVulnerabilitiesCount")));

        result.getArtifactArchiveListing()
                .setFilenames(extractPropertyList(String.class, t.get().get("filenames")).stream()
                        .filter(e -> !e.trim().isEmpty())
                        .collect(Collectors.toSet()));

        result.addChecksums(extractPropertyList(String.class, t.get().get("checksums")).stream()
                .filter(e -> !e.trim().isEmpty())
                .collect(Collectors.toSet()));

        List<ArtifactTag> tags = (List<ArtifactTag>) t.get().get("tags");
        result.setTagSet(new HashSet<>(tags));

        Object vulnerabilityObject = t.get().get("vulnerabilitySet");
        if (Objects.nonNull(vulnerabilityObject)) {
            List<Vulnerability> vulnerabilityList = (List<Vulnerability>) vulnerabilityObject;
            result.setVulnerabilitySet(new LinkedHashSet<>(vulnerabilityList));
        }

        result.setArtifactFileExists(extractObject(Boolean.class, t.get().get("artifactFileExists")));

        return result;
    }

    private Artifact map(Traverser<Map<String, Object>> t) {
        String storageId = extractObject(String.class, t.get().get("storageId"));
        String repositoryId = extractObject(String.class, t.get().get("repositoryId"));
        ArtifactCoordinates artifactCoordinates = null;
        if (t.get().get("artifactCoordinates") instanceof GenericCoordinatesEntity entity) {
            Repository repository = configurationManager.getRepository(storageId, repositoryId);
            if (repository == null) {
                throw new IllegalStateException("Repository not found: " + storageId + ":" + repositoryId);
            }
            Class<? extends GenericCoordinates> clazz = Optional.ofNullable(repository.getLayout())
                    .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                    .map(ArtifactLayoutDescription::getArtifactCoordinatesClass)
                    .orElseThrow(() -> new IllegalStateException("Coordinates class not found"));

            artifactCoordinates =   strategyFactory.getArtifactCoordinates(clazz, entity);
        } else {
            artifactCoordinates = extractObject(ArtifactCoordinates.class,
                    t.get().get("artifactCoordinates"));
        }

        ArtifactEntity result = new ArtifactEntity(storageId, repositoryId, artifactCoordinates);
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        result.setCreated(toLocalDateTime(extractObject(Long.class, t.get().get("created"))));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setLastUsed(toLocalDateTime(extractObject(Long.class, t.get().get("lastUsed"))));
        result.setSizeInBytes(extractObject(Long.class, t.get().get("sizeInBytes")));
        result.setDownloadCount(extractObject(Integer.class, t.get().get("downloadCount")));

        result.setSafeLevel(extractObject(String.class, t.get().get("safeLevel")));
        result.setEvidenceQuantity(extractObject(Integer.class, t.get().get("evidenceQuantity")));
        result.setVulnerabilities(extractPropertyList(String.class, t.get().get("vulnerabilities")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        result.setDependencyCount(extractObject(Integer.class, t.get().get("dependencyCount")));
        result.setDependencyVulnerabilitiesCount(extractObject(Integer.class, t.get().get("dependencyVulnerabilitiesCount")));
        result.setVulnerabilitiesCount(extractObject(Integer.class, t.get().get("vulnerabilitiesCount")));
        result.setCriticalVulnerabilitiesCount(extractObject(Integer.class, t.get().get("criticalVulnerabilitiesCount")));
        result.setHighVulnerabilitiesCount(extractObject(Integer.class, t.get().get("highVulnerabilitiesCount")));
        result.setMediumVulnerabilitiesCount(extractObject(Integer.class, t.get().get("mediumVulnerabilitiesCount")));
        result.setLowVulnerabilitiesCount(extractObject(Integer.class, t.get().get("lowVulnerabilitiesCount")));
        result.setSuppressedVulnerabilitiesCount(extractObject(Integer.class, t.get().get("suppressedVulnerabilitiesCount")));
        result.setDependencies(extractObject(String.class, t.get().get("dependencies")));

        result.getArtifactArchiveListing()
                .setFilenames(extractPropertyList(String.class, t.get().get("filenames")).stream()
                        .filter(e -> !e.trim().isEmpty())
                        .collect(Collectors.toSet()));

        result.addChecksums(extractPropertyList(String.class, t.get().get("checksums")).stream()
                .filter(e -> !e.trim().isEmpty())
                .collect(Collectors.toSet()));

        Object tagObject = t.get().get("tags");
        if (Objects.nonNull(tagObject)) {
            List<ArtifactTag> tags = (List<ArtifactTag>) tagObject;
            result.setTagSet(new HashSet<>(tags));
        }

        Object vulnerabilityObject = t.get().get("vulnerabilitySet");
        if (Objects.nonNull(vulnerabilityObject)) {
            List<Vulnerability> vulnerabilityList = (List<Vulnerability>) vulnerabilityObject;
            result.setVulnerabilitySet(new LinkedHashSet<>(vulnerabilityList));
        }
        Object componentObject = t.get().get("componentSet");
        if (Objects.nonNull(componentObject)) {
            List<com.folib.domain.Component> componentList = (List<com.folib.domain.Component>) componentObject;
            result.setComponentSet(new LinkedHashSet<>(componentList));
        }
        result.setArtifactFileExists(extractObject(Boolean.class, t.get().get("artifactFileExists")));
        result.setMetadata(extractObject(String.class, t.get().get("metadata")));
        result.setFilePaths(extractPropertyList(String.class, t.get().get("filePaths")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        result.setReport(extractObject(String.class, t.get().get("report")));
        result.setScanDate(extractObject(String.class, t.get().get("scanDate")));
        result.setScanDateTime(toLocalDateTime(extractObject(Long.class, t.get().get("scanDateTime"))));
        result.setPromotion(extractObject(String.class, t.get().get("promotion")));
        result.setPromotionNodes(extractPropertyList(String.class, t.get().get("promotionNodes")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        result.setEnabled(extractObject(Boolean.class, t.get().get("enabled")));
        result.setCreatedBy(extractObject(String.class, t.get().get("createdBy")));
        result.setUpdatedBy(extractObject(String.class, t.get().get("updatedBy")));
        result.setArtifactName(extractObject(String.class, t.get().get("artifactName")));
        result.setArtifactPath(extractObject(String.class, t.get().get("artifactPath")));
        result.setPackageInfo(extractObject(String.class, t.get().get("packageInfo")));
        result.setLayers(extractPropertyList(String.class, t.get().get("layers")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toSet()));
        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(Artifact entity) {
        ArtifactCoordinates artifactCoordinates = entity.getArtifactCoordinates();
        String storedArtifactId = Vertices.ARTIFACT + ":" + UUID.randomUUID().toString();

        Set<String> tagNames = entity.getTagSet().stream().map(ArtifactTag::getName).collect(Collectors.toSet());
        Set<String> vulnerabilities = entity.getVulnerabilities();

        EntityTraversal<Vertex, Vertex> unfoldTraversal = __.<Vertex, Edge>coalesce(__.<Vertex>outE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES),
                //cascading create ArtifactCoordinates only
                createArtifactCoordinates(artifactCoordinates))
                .outV()
                .map(unfoldArtifact(entity))
                .store(storedArtifactId);
        if (CollectionUtils.isNotEmpty(tagNames)) {
            unfoldTraversal = unfoldTraversal.sideEffect(__.outE(Edges.ARTIFACT_HAS_TAGS).drop());
            unfoldTraversal = unfoldTraversal.sideEffect(__.V()
                    .hasLabel(Vertices.ARTIFACT_TAG)
                    .has(Properties.UUID, P.within(tagNames))
                    .addE(Edges.ARTIFACT_HAS_TAGS)
                    .from(__.select(storedArtifactId).unfold()));
        }
        String drop = "drop";
        if (CollectionUtils.isNotEmpty(vulnerabilities)) {
            unfoldTraversal = unfoldTraversal.sideEffect(__.outE(Edges.ARTIFACT_HAS_VULNERABILITIES).drop());
            for (String vulnerability : vulnerabilities) {
                if (drop.equalsIgnoreCase(vulnerability)) {
                    break;
                }
                EntityTraversal<Object, Vertex> vulnerabilityEntityTraversal = __.V()
                        .hasLabel(Vertices.VULNERABILITY)
                        .has(Properties.UUID, vulnerability);
                vulnerabilityEntityTraversal = vulnerabilityEntityTraversal.addE(Edges.ARTIFACT_HAS_VULNERABILITIES)
                        .from(__.<Vertex, Vertex>select(storedArtifactId).unfold())
                        .property(Properties.VULNERABILITY_ID, vulnerability).inV();
                unfoldTraversal = unfoldTraversal.sideEffect(vulnerabilityEntityTraversal);
            }
        }
        if (CollectionUtils.isNotEmpty(entity.getComponentSet())) {
            unfoldTraversal = unfoldTraversal.sideEffect(__.outE(Edges.ARTIFACT_HAS_COMPONENTS).drop());
            for (com.folib.domain.Component component : entity.getComponentSet()) {
                if (drop.equalsIgnoreCase(component.getUuid())) {
                    break;
                }
                EntityTraversal<Object, Vertex> componentEntityTraversal = __.V()
                        .hasLabel(Vertices.COMPONENT)
                        .has(Properties.UUID, component.getUuid());
                componentEntityTraversal = componentEntityTraversal.addE(Edges.ARTIFACT_HAS_COMPONENTS)
                        .from(__.<Vertex, Vertex>select(storedArtifactId).unfold())
                        .property(Properties.UUID, entity.getUuid()).inV();
                unfoldTraversal = unfoldTraversal.sideEffect(componentEntityTraversal);
            }
        }
        return new UnfoldEntityTraversal<>(Vertices.ARTIFACT, entity, unfoldTraversal);
    }

    private Traversal<Vertex, Edge> createArtifactCoordinates(ArtifactCoordinates artifactCoordinates) {
        return __.<Vertex>addE(Edges.ARTIFACT_HAS_ARTIFACT_COORDINATES)
                .to(saveArtifactCoordinates(artifactCoordinates));
    }

    private <S2> EntityTraversal<S2, Vertex> saveArtifactCoordinates(ArtifactCoordinates artifactCoordinates) {
        UnfoldEntityTraversal<Vertex, Vertex> artifactCoordinatesUnfold = artifactCoordinatesAdapter.unfold(artifactCoordinates);

        return __.<S2>V(artifactCoordinates)
                .saveV(artifactCoordinates.getUuid(),
                        artifactCoordinatesUnfold);
    }

    private EntityTraversal<Vertex, Vertex> unfoldArtifact(Artifact entity) {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (StringUtils.isNotBlank(entity.getStorageId())) {
            t = t.property(single, "storageId", entity.getStorageId());
        }
        if (StringUtils.isNotBlank(entity.getRepositoryId())) {
            t = t.property(single, "repositoryId", entity.getRepositoryId());
        }
        if (StringUtils.isNotBlank(entity.getStorageIdAndRepositoryId())) {
            t = t.property(single, "storageIdAndRepositoryId", entity.getStorageIdAndRepositoryId());
        }
        if (Objects.nonNull(entity.getCreated())) {
            t = t.property(single, "created", toLong(entity.getCreated()));
        }
        if (Objects.nonNull(entity.getLastUpdated())) {
            t = t.property(single, "lastUpdated", toLong(entity.getLastUpdated()));
        }
        if (Objects.nonNull(entity.getLastUsed())) {
            t = t.property(single, "lastUsed", toLong(entity.getLastUsed()));
        }
        if (Objects.nonNull(entity.getSizeInBytes())) {
            t = t.property(single, "sizeInBytes", entity.getSizeInBytes());
        }
        log.debug("[{}] downloadCount changed [{}]", entity.getUuid(), entity.getDownloadCount());
        if (Objects.nonNull(entity.getDownloadCount())) {
            t = t.property(single, "downloadCount", entity.getDownloadCount());
        }
        if (StringUtils.isNotBlank(entity.getSafeLevel())) {
            t = t.property(single, "safeLevel", entity.getSafeLevel());
        }
        if (Objects.nonNull(entity.getDependencyCount())) {
            t = t.property(single, "dependencyCount", entity.getDependencyCount());
        }
        if (Objects.nonNull(entity.getDependencyVulnerabilitiesCount())) {
            t = t.property(single, "dependencyVulnerabilitiesCount", entity.getDependencyVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getVulnerabilitiesCount())) {
            t = t.property(single, "vulnerabilitiesCount", entity.getVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getCriticalVulnerabilitiesCount())) {
            t = t.property(single, "criticalVulnerabilitiesCount", entity.getCriticalVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getHighVulnerabilitiesCount())) {
            t = t.property(single, "highVulnerabilitiesCount", entity.getHighVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getMediumVulnerabilitiesCount())) {
            t = t.property(single, "mediumVulnerabilitiesCount", entity.getMediumVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getLowVulnerabilitiesCount())) {
            t = t.property(single, "lowVulnerabilitiesCount", entity.getLowVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getSuppressedVulnerabilitiesCount())) {
            t = t.property(single, "suppressedVulnerabilitiesCount", entity.getSuppressedVulnerabilitiesCount());
        }
        if (Objects.nonNull(entity.getEvidenceQuantity())) {
            t = t.property(single, "evidenceQuantity", entity.getEvidenceQuantity());
        }
        String drop = "drop";
        if (CollectionUtils.isNotEmpty(entity.getVulnerabilities())) {
            t = t.sideEffect(__.properties("vulnerabilities").drop());
            if (!entity.getVulnerabilities().contains(drop)) {
                t = t.property("vulnerabilities", entity.getVulnerabilities());
            }
        }
        if (StringUtils.isNotBlank(entity.getMetadata())) {
            log.info("Artifact [{}] metadata change to [{}]", entity.getUuid(), entity.getMetadata());
            t = t.property(single, "metadata", entity.getMetadata());
        }
        if (CollectionUtils.isNotEmpty(entity.getFilePaths())) {
            t = t.sideEffect(__.properties("filePaths").drop());
            t = t.property("filePaths", entity.getFilePaths());
        }
        if (StringUtils.isNotBlank(entity.getReport())) {
            t = t.property(single, "report", entity.getReport());
        }
        if (StringUtils.isNotBlank(entity.getScanDate())) {
            t = t.property(single, "scanDate", entity.getScanDate());
        }
        if (Objects.nonNull(entity.getScanDateTime())) {
            t = t.property(single, "scanDateTime", toLong(entity.getScanDateTime()));
        }
        if (StringUtils.isNotBlank(entity.getDependencies())) {
            t = t.property(single, "dependencies", entity.getDependencies());
        }
        ArtifactArchiveListing artifactArchiveListing = entity.getArtifactArchiveListing();
        if (Objects.nonNull(artifactArchiveListing)) {
            Set<String> filenames = artifactArchiveListing.getFilenames();
            if (CollectionUtils.isNotEmpty(filenames)) {
                t = t.sideEffect(__.properties("filenames").drop());
                t = t.property("filenames", filenames);
            }
        }
        Map<String, String> checksums = entity.getChecksums();
        if (MapUtils.isNotEmpty(checksums)) {
            Set<String> checkSumAlgo = new HashSet<>();
            for (String alg : checksums.keySet()) {
                checkSumAlgo.add("{" + alg + "}" + checksums.get(alg));
            }
            t = t.sideEffect(__.properties("checksums").drop());
            t = t.property("checksums", checkSumAlgo);
        }
        if (Objects.nonNull(entity.getArtifactFileExists())) {
            t = t.property(single, "artifactFileExists", entity.getArtifactFileExists());
        }
        if (StringUtils.isNotBlank(entity.getPromotion())) {
            if (drop.equals(entity.getPromotion())) {
                t = t.property(single, "promotion", "");
            } else {
                t = t.property(single, "promotion", entity.getPromotion());
            }
        }
        if (CollectionUtils.isNotEmpty(entity.getPromotionNodes())) {
            t = t.sideEffect(__.properties("promotionNodes").drop());
            if (!entity.getPromotionNodes().contains(drop)) {
                t = t.property("promotionNodes", entity.getPromotionNodes());
            }
        }
        if (Objects.nonNull(entity.getEnabled())) {
            t = t.property(single, "enabled", entity.getEnabled());
        }
        if (StringUtils.isNotBlank(entity.getCreatedBy())) {
            t = t.property(single, "createdBy", entity.getCreatedBy());
        }
        if (StringUtils.isBlank(entity.getUpdatedBy())) {
            entity.setUpdatedBy(UserUtils.getUsername());
        }
        if (StringUtils.isNotBlank(entity.getUpdatedBy())) {
            t = t.property(single, "updatedBy", entity.getUpdatedBy());
        }
        if (StringUtils.isNotBlank(entity.getArtifactName())) {
            t = t.property(single, "artifactName", entity.getArtifactName());
        }
        if (StringUtils.isNotBlank(entity.getArtifactPath())) {
            t = t.property(single, "artifactPath", entity.getArtifactPath());
        }
        if (StringUtils.isNotBlank(entity.getPackageInfo())) {
            t = t.property(single, "packageInfo", entity.getPackageInfo());
        }
        if (CollectionUtils.isNotEmpty(entity.getLayers())) {
            t = t.sideEffect(__.properties("layers").drop());
            t = t.property("layers", entity.getLayers());
        }
        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade() {
        return __.<Vertex>aggregate("x")
                .select("x")
                .unfold();
    }

    public Artifact mapFromMap(Map<String, Object> data)
    {
        ArtifactEntity result = new ArtifactEntity();
        result.setNativeId(extractObject(Long.class, data.get("id")));
        result.setUuid(extractObject(String.class, data.get("uuid")));
        result.setStorageId(extractObject(String.class, data.get("storageId")));
        result.setRepositoryId(extractObject(String.class, data.get("repositoryId")));
        result.setStorageIdAndRepositoryId(extractObject(String.class, data.get("storageIdAndRepositoryId")));
        result.setLastUpdated(extractObject(LocalDateTime.class, data.get("lastUpdated")));
        result.setLastUsed(extractObject(LocalDateTime.class, data.get("lastUsed")));
        System.out.println("result.created() = " + JSON.toJSONString(data.get("created")));

        if (data.get("created") instanceof Long created) {
            LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(created), ZoneId.systemDefault());
            result.setCreated(dateTime);
        }
        result.setSizeInBytes(extractObject(Long.class, data.get("sizeInBytes")));
        result.setDownloadCount(extractObject(Integer.class, data.get("downloadCount")));
        result.setSafeLevel(extractObject(String.class, data.get("safeLevel")));
        result.setFilePaths(extractObject(Set.class, data.get("filePaths")));
        result.setArtifactCoordinates(extractObject(ArtifactCoordinates.class, data.get("artifactCoordinates")));
        result.setTagSet(extractObject(Set.class, data.get("tagSet")));

        result.setArtifactFileExists(extractObject(Boolean.class, data.get("artifactFileExists")));
        result.setDependencyCount(extractObject(Integer.class, data.get("dependencyCount")));
        result.setPromotion(extractObject(String.class, data.get("promotion")));
        result.setPromotionNodes(extractObject(Set.class, data.get("promotionNodes")));
        result.setEnabled(extractObject(Boolean.class, data.get("enabled")));
        result.setComponentSet(extractObject(Set.class, data.get("componentSet")));
        result.setCreatedBy(extractObject(String.class, data.get("createdBy")));
        result.setUpdatedBy(extractObject(String.class, data.get("updatedBy")));
        result.setArtifactName(extractObject(String.class, data.get("artifactName")));
        result.setArtifactPath(extractObject(String.class, data.get("artifactPath")));
        result.setScanDateTime(extractObject(LocalDateTime.class, data.get("scanDateTime")));
        result.setMetadata(extractObject(String.class, data.get("metadata")));
        return result;
    }



}
