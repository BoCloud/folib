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

import com.folib.components.DistributedLockComponent;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.domain.Component;
import com.folib.gremlin.adapters.ArtifactAdapter;
import com.folib.gremlin.adapters.ComponentAdapter;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.EntityTraversalUtils;
import com.folib.gremlin.repositories.GremlinVertexRepository;
import com.folib.util.CommonUtils;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.JanusGraph;
import org.janusgraph.core.attribute.Text;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import jakarta.inject.Inject;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * 组件顶点图数据交互
 *
 * @author veadan
 **/
@Slf4j
@Repository
@Transactional
public class ComponentRepository extends GremlinVertexRepository<Component> {

    @Inject
    private ComponentAdapter componentAdapter;

    @Inject
    private ArtifactAdapter artifactAdapter;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private JanusGraph janusGraph;

    @Inject
    DistributedLockComponent distributedLockComponent;

    @Override
    protected ComponentAdapter adapter() {
        return componentAdapter;
    }

    public void saveOrUpdate(Component component) {
        if (distributedLockComponent.lock(component.getUuid(), GlobalConstants.WAIT_LOCK_TIME, TimeUnit.SECONDS)) {
            try {
                try {
                    merge(component);
                } catch (Exception ex) {
                    if (CommonUtils.catchException(ex)) {
                        log.warn("Handle component [{}] catch error", component.getUuid());
                        return;
                    }
                    log.error("Handle component [{}] error [{}]", component.getUuid(), ExceptionUtils.getStackTrace(ex));
                    throw new RuntimeException(ex.getMessage());
                }
            } finally {
                distributedLockComponent.unLock(component.getUuid());
            }
        } else {
            log.warn("Handle component [{}] was not get lock", component.getUuid());
        }
    }

    public Page<Component> queryComponentPage(Pageable pagination, String name, String groupId, String version, String fileName) {
        Long count = commonBuildEntityTraversal(name, groupId, version, fileName).count().tryNext().orElse(0L);
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Component> componentList = commonBuildEntityTraversal(name, groupId, version, fileName)
                .range(low, high)
                .map(componentAdapter.fold()).toList();
        return new PageImpl<Component>(EntityTraversalUtils.reduceHierarchy(componentList), pagination, count);
    }

    public Page<Component> queryComponentPageByArtifact(Pageable pagination, String artifactPath,
                                                        String fileName) {
        Long count = commonBuildEntityTraversalByArtifact(artifactPath, fileName).count().tryNext().orElse(0L);
        long low = pagination.getPageNumber() * pagination.getPageSize();
        long high = (pagination.getPageNumber() + 1) * pagination.getPageSize();
        List<Component> componentList = commonBuildEntityTraversalByArtifact(artifactPath, fileName)
                .range(low, high)
                .map(componentAdapter.fold()).toList();
        return new PageImpl<Component>(EntityTraversalUtils.reduceHierarchy(componentList), pagination, count);
    }

    /**
     * 构建公共图查询
     *
     * @param fileName fileName
     * @return 公共图查询
     */
    private EntityTraversal<Vertex, Vertex> commonBuildEntityTraversal(String name, String groupId, String version, String fileName) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.COMPONENT).has(Properties.CREATED, P.gt(0));
        if (StringUtils.isNotBlank(name)) {
            entityTraversal = entityTraversal.has(Properties.NAME, Text.textContains(name));
        }
        if (StringUtils.isNotBlank(groupId)) {
            entityTraversal = entityTraversal.has(Properties.GROUP_ID, Text.textContains(groupId));
        }
        if (StringUtils.isNotBlank(version)) {
            entityTraversal = entityTraversal.has(Properties.VERSION, Text.textContains(version));
        }
        if (StringUtils.isNotBlank(fileName)) {
            entityTraversal = entityTraversal.has(Properties.FILE_NAME, Text.textContains(fileName));
        }
        return entityTraversal;
    }

    /**
     * 构建公共图查询
     *
     * @param fileName fileName
     * @return 公共图查询
     */
    private EntityTraversal<Vertex, Vertex> commonBuildEntityTraversalByArtifact(String artifactPath, String fileName) {
        EntityTraversal<Vertex, Vertex> entityTraversal = g().V().hasLabel(Vertices.ARTIFACT).has(Properties.UUID, artifactPath).outE(Edges.ARTIFACT_HAS_COMPONENTS).inV();
        if (StringUtils.isNotBlank(fileName)) {
            entityTraversal = entityTraversal.has(Properties.FILE_NAME, Text.textContains(fileName));
        }
        return entityTraversal;
    }
}

//@Repository
//interface ComponentQueries
//        extends org.springframework.data.repository.Repository<Component, String> {
//
//}