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
package com.folib.repositories;

import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.ArtifactLayoutDescription;
import com.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.folib.components.DistributedLockComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.gremlin.adapters.ArtifactAdapter;
import com.folib.gremlin.adapters.ArtifactIdGroupAdapter;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import com.folib.util.CommonUtils;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.attribute.Text;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import jakarta.inject.Inject;
import java.util.*;
import java.util.concurrent.TimeUnit;

@Repository
@Transactional
@Slf4j
public class ArtifactIdGroupRepository extends GremlinVertexRepository<ArtifactIdGroup> {

    @Inject
    ArtifactIdGroupAdapter adapter;
    @Inject
    ConfigurationManager configurationManager;
    @Inject
    @Lazy
    ArtifactAdapter artifactAdapter;
    @Inject
    DistributedLockComponent distributedLockComponent;

    public void saveOrUpdate(ArtifactIdGroup artifactIdGroup) {
        if (distributedLockComponent.lock(artifactIdGroup.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                try {
                    merge(artifactIdGroup);
                } catch (Exception ex) {
                    if (CommonUtils.catchException(ex)) {
                        log.warn("Handle artifactIdGroup [{}] catch error", artifactIdGroup.getUuid());
                        return;
                    }
                    log.error("Handle artifactIdGroup [{}] error [{}]", artifactIdGroup.getUuid(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
            } finally {
                distributedLockComponent.unLock(artifactIdGroup.getUuid());
            }
        } else {
            log.warn("Handle artifactIdGroup [{}] was not get lock", artifactIdGroup.getUuid());
        }
    }

    @Override
    protected ArtifactIdGroupAdapter adapter() {
        return adapter;
    }

    public Page<ArtifactIdGroup> findMatching(String storageId,
                                              String repositoryId,
                                              Pageable page) {
        Long count = commonArtifactIdGroupPage(storageId, repositoryId).count().tryNext().orElse(0L);
        com.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        long low = page.getPageNumber() * page.getPageSize();
        long high = (page.getPageNumber() + 1) * page.getPageSize();
        List<ArtifactIdGroup> artifactIdGroupList = commonArtifactIdGroupPage(storageId, repositoryId)
                .map(adapter.fold(Optional.ofNullable(repository)
                                .map(com.folib.storage.repository.Repository::getLayout)
                                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass),
                        Optional.empty())).range(low, high).toList();
        return new PageImpl<>(artifactIdGroupList, page, count);
    }

    private EntityTraversal<Vertex, Vertex> commonArtifactIdGroupPage(String storageId, String repositoryId) {
        return g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP)
                .has(Properties.STORAGE_ID, storageId)
                .has(Properties.REPOSITORY_ID, repositoryId);
    }

    public Optional<ArtifactIdGroup> findAllArtifactsInGroup(String storageId,
                                                             String repositoryId,
                                                             String artifactId) {
        return findArtifactsGroupWithTag(storageId, repositoryId, artifactId, Optional.empty());
    }

    public Optional<ArtifactIdGroup> findArtifactsGroupWithTag(String storageId,
                                                               String repositoryId,
                                                               String artifactId,
                                                               Optional<ArtifactTag> tag) {
        com.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);

        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
        EntityTraversal<Vertex, ArtifactIdGroup> t = g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP)
                .has("uuid", artifactIdGroup.getUuid())
                .map(adapter.fold(Optional.ofNullable(repository)
                                .map(com.folib.storage.repository.Repository::getLayout)
                                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass),
                        tag));
        if (!t.hasNext()) {
            return Optional.empty();
        }

        return Optional.of(t.next());
    }

    public Optional<ArtifactIdGroup> findArtifactGroupWithTag(String storageId,
                                                              String repositoryId,
                                                              String artifactId,
                                                              Optional<ArtifactTag> tag) {
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
        EntityTraversal<Vertex, ArtifactIdGroup> t = g().V()
                .hasLabel(Vertices.ARTIFACT_ID_GROUP)
                .has("uuid", artifactIdGroup.getUuid())
                .map(adapter.artifactIdGroupFold());
        if (!t.hasNext()) {
            return Optional.empty();
        }

        return Optional.of(t.next());
    }

    public List<Artifact> findArtifactsGremlin(String storageId,
                                               String repositoryId,
                                               String artifactId,
                                               Collection<String> coordinateValues,
                                               Long skip,
                                               Integer limit,
                                               Boolean useLimit) {
        return findArtifactsGremlin(storageId, repositoryId, artifactId, false, coordinateValues, skip, limit, useLimit);
    }

    public List<Artifact> findArtifactsGremlin(String storageId,
                                               String repositoryId,
                                               String artifactId,
                                               Boolean useArtifactName,
                                               Collection<String> coordinateValues,
                                               Long skip,
                                               Integer limit,
                                               Boolean useLimit) {
        if (Boolean.FALSE.equals(useLimit)) {
            skip = 0L;
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            long startTime = System.currentTimeMillis();
            Long count = commonSearchCountArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues);
            log.info("ArtifactIdGroup [{}] commonSearchCountArtifacts count [{}] take time [{}] ms", artifactIdGroup.getUuid(), count, System.currentTimeMillis() - startTime);
            if (Objects.isNull(count) || skip.equals(count)) {
                startTime = System.currentTimeMillis();
                count = commonCountArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues);
                log.info("ArtifactIdGroup [{}] commonCountArtifacts count [{}] take time [{}] ms", artifactIdGroup.getUuid(), count, System.currentTimeMillis() - startTime);
            }
            if (count > 0L) {
                limit = count.intValue();
            }
        }
        com.folib.storage.repository.Repository repository = configurationManager.getRepository(storageId, repositoryId);
        long startTime = System.currentTimeMillis();
        List<Artifact> artifactList = commonFindArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues).order().by(Properties.CREATED, Order.asc).map(artifactAdapter.fold(Optional.ofNullable(repository)
                .map(com.folib.storage.repository.Repository::getLayout)
                .map(ArtifactLayoutLocator.getLayoutByNameEntityMap()::get)
                .map(ArtifactLayoutDescription::getArtifactCoordinatesClass))).range(skip, limit).toList();
        if (CollectionUtils.isEmpty(artifactList)) {
            artifactList = Collections.emptyList();
        }
        log.debug("FindArtifactsGremlin storageId [{}] repositoryId [{}] artifactId [{}] coordinateValues [{}] skip [{}] limit [{}] useLimit [{}] artifactListSize [{}] take time [{}] ms", storageId, repositoryId, artifactId, coordinateValues, skip, limit, useLimit, artifactList.size(), System.currentTimeMillis() - startTime);
        return artifactList;
    }

    public long commonCountArtifacts(String storageId,
                                     String repositoryId,
                                     String artifactId,
                                     Collection<String> coordinateValues) {
        return commonFindArtifacts(storageId, repositoryId, artifactId, false, coordinateValues).count().tryNext().orElse(0L);
    }

    public long commonCountArtifacts(String storageId,
                                     String repositoryId,
                                     String artifactId,
                                     Boolean useArtifactName,
                                     Collection<String> coordinateValues) {
        return commonFindArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues).count().tryNext().orElse(0L);
    }

    public Boolean commonArtifactsExists(String storageId,
                                         String repositoryId,
                                         String artifactId,
                                         Collection<String> coordinateValues) {
        return commonCountArtifacts(storageId, repositoryId, artifactId, false, coordinateValues) > 0L;
    }

    public Boolean commonArtifactsExists(String storageId,
                                         String repositoryId,
                                         String artifactId,
                                         Boolean useArtifactName,
                                         Collection<String> coordinateValues) {
        return commonCountArtifacts(storageId, repositoryId, artifactId, useArtifactName, coordinateValues) > 0L;
    }

    public static String toCaseInsensitiveRegex(String regex) {
        StringBuilder caseInsensitiveRegex = new StringBuilder();
        for (char c : regex.toCharArray()) {
            if (Character.isLetter(c)) {
                caseInsensitiveRegex.append("[").append(Character.toLowerCase(c)).append(Character.toUpperCase(c)).append("]");
            } else {
                caseInsensitiveRegex.append(c);
            }
        }
        return caseInsensitiveRegex.toString();
    }

    private EntityTraversal<Vertex, Vertex> commonFindArtifacts(String storageId,
                                                                String repositoryId,
                                                                String artifactId,
                                                                Boolean useArtifactName,
                                                                Collection<String> coordinateValues) {
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        EntityTraversal<Vertex, Vertex> t = null;
        if (Boolean.TRUE.equals(useArtifactName)) {
            artifactId = artifactId  + ".*";
            artifactId = toCaseInsensitiveRegex(artifactId);
            t = g().V()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId).has(Properties.ARTIFACT_NAME, Text.textRegex(artifactId));
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                handleCoordinateValues(t, coordinateValues, true);
            }
        } else {
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            t = g().V()
                    .hasLabel(Vertices.ARTIFACT_ID_GROUP).has(Properties.UUID, artifactIdGroup.getUuid()).outE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS).inV()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId).has(Properties.UUID, Text.textPrefix(artifactIdGroup.getUuid()));
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                handleCoordinateValues(t, coordinateValues, false);
            }
        }
        return t;
    }

    /**
     * 统计 走搜索引擎
     *
     * @param storageId        storageId
     * @param repositoryId     repositoryId
     * @param artifactId       artifactId
     * @param coordinateValues coordinateValues
     * @return 个数
     */
    public Long commonSearchCountArtifacts(String storageId,
                                           String repositoryId,
                                           String artifactId,
                                           Boolean useArtifactName,
                                           Collection<String> coordinateValues) {
        String storageIdAndRepositoryId = String.format("%s-%s", storageId, repositoryId);
        if (Boolean.TRUE.equals(useArtifactName)) {
            artifactId = artifactId  + ".*";
            artifactId = toCaseInsensitiveRegex(artifactId);
            EntityTraversal<Vertex, Vertex> t = g().V()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId)
                    .has(Properties.ARTIFACT_NAME, Text.textRegex(artifactId));
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                handleCoordinateValues(t, coordinateValues, true);
            }
            return t.count().tryNext().orElse(0L);
        } else {
            ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, artifactId);
            EntityTraversal<Vertex, Vertex> t = g().V()
                    .hasLabel(Vertices.ARTIFACT).has(Properties.STORAGE_ID_AND_REPOSITORY_ID, storageIdAndRepositoryId);
            String regex = "(%s/)", suffix = "";
            //(folib-common-taobao-npm-vue)(.*tgz.*)
            regex = String.format(regex, artifactIdGroup.getUuid());
            if (CollectionUtils.isNotEmpty(coordinateValues)) {
                suffix = "(.*.(%s))";
                suffix = String.format(suffix, String.join("|", coordinateValues));
                regex = regex + suffix;
            }
            t.has(Properties.UUID, Text.textRegex(regex));
            return t.count().tryNext().orElse(0L);
        }
    }

    public ArtifactIdGroup findByArtifactIdGroup(String artifactIdGroup) {
        return g().V().hasLabel(Vertices.ARTIFACT_ID_GROUP).has(Properties.UUID, artifactIdGroup).map(adapter.artifactIdGroupFold()).tryNext().orElse(null);
    }

    private void handleCoordinateValues(EntityTraversal<Vertex, Vertex> t, Collection<String> coordinateValues, Boolean useArtifactName) {
        String s = ".*.(%s)";
        final String regex = String.format(s, String.join("|", coordinateValues));
        if (Boolean.TRUE.equals(useArtifactName)) {
            t.has(Properties.ARTIFACT_NAME, Text.textRegex(regex));
        } else {
            t.has(Properties.UUID, Text.textRegex(regex));
        }
    }

}