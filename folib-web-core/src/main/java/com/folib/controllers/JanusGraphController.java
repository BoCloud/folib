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
package com.folib.controllers;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.folib.components.node.NodeComponent;
import com.folib.db.schema.util.SchemaUtils;
import com.folib.domain.JanusGraphIndex;
import com.folib.util.CommonUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.janusgraph.core.JanusGraph;
import org.janusgraph.core.PropertyKey;
import org.janusgraph.core.RelationType;
import org.janusgraph.core.schema.JanusGraphManagement;
import org.janusgraph.core.schema.RelationTypeIndex;
import org.janusgraph.core.schema.SchemaStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * @author veadan
 */
@Slf4j
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping("/api/janusGraph")
@Api(description = "janusGraph图库管理", tags = "janusGraph图库管理")
public class JanusGraphController extends BaseController {

    @Inject
    private JanusGraph janusGraph;

    @Inject
    private NodeComponent nodeComponent;

    @ApiOperation(value = "删除指定实例")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @DeleteMapping(value = "/instance/{instanceId}")
    public void deleteInstance(@PathVariable(name = "instanceId") String instanceId) {
        String current = "(current)";
        if (instanceId.contains(current)) {
            return;
        }
        JanusGraphManagement janusGraphManagement = janusGraph.openManagement();
        try {
            Set<String> ids = janusGraphManagement.getOpenInstances();
            log.info("所有实例：{}，要删除的实例：{}", ids, instanceId);
            janusGraphManagement.forceCloseInstance(instanceId);
            janusGraphManagement.commit();
        } catch (Exception ex) {
            log.error("删除实例异常：", ex);
            janusGraphManagement.rollback();
            throw new RuntimeException(ex);
        }
    }

    @ApiOperation(value = "查询janusGraph信息")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @GetMapping
    public ResponseEntity<JSONObject> janusGraphInfo() {
        JSONObject data = new JSONObject();
        JanusGraphManagement janusGraphManagement = janusGraph.openManagement();
        try {
            int clusterNodeTotal = nodeComponent.getClusterNodeTotal();
            Set<String> openInstances = janusGraphManagement.getOpenInstances();
            data.put("openInstances", openInstances);
            data.put("clusterNodeTotal", clusterNodeTotal);
            data.put("instanceStatus", clusterNodeTotal == openInstances.size() ? "normal" : "exceptional");
            data.put("schema", janusGraphManagement.printSchema());
            Set<String> vertexIndexes = fetchVertexIndexes(janusGraphManagement);
            org.janusgraph.core.schema.JanusGraphIndex janusGraphIndex;
            PropertyKey[] propertyKeys;
            SchemaStatus schemaStatus;
            List<String> normalList = Lists.newArrayList(SchemaStatus.DISABLED.name(), SchemaStatus.ENABLED.name());
            Set<String> exceptionalIndexSet = Sets.newLinkedHashSet();
            for (String janusGraphIndexName : vertexIndexes) {
                janusGraphIndex = janusGraphManagement.getGraphIndex(janusGraphIndexName);
                propertyKeys = janusGraphIndex.getFieldKeys();
                for (PropertyKey propertyKey : propertyKeys) {
                    schemaStatus = janusGraphIndex.getIndexStatus(propertyKey);
                    if (!normalList.contains(schemaStatus.name())) {
                        exceptionalIndexSet.add(janusGraphIndexName);
                        break;
                    }
                }
            }
            Map<String, String> relationIndexes = fetchRelationIndexes(janusGraphManagement);
            RelationTypeIndex relationTypeIndex;
            for (Map.Entry<String, String> e : relationIndexes.entrySet()) {
                relationTypeIndex = janusGraphManagement.getRelationIndex(janusGraphManagement.getRelationType(e.getValue()), e.getKey());
                schemaStatus = relationTypeIndex.getIndexStatus();
                if (!normalList.contains(schemaStatus.name())) {
                    exceptionalIndexSet.add(e.getKey());
                }
            }
            data.put("exceptionalIndexSet", exceptionalIndexSet);
            janusGraphManagement.rollback();
        } catch (Exception ex) {
            log.error("查询janusGraph信息异常：", ex);
            janusGraphManagement.rollback();
            throw new RuntimeException(ex);
        }
        return ResponseEntity.ok(data);
    }

    @ApiOperation(value = "重建索引")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = "/reindex")
    public void reindex(@RequestBody JanusGraphIndex janusGraphIndex) {
        try {
            log.info("重建索引：{}", janusGraphIndex);
            SchemaUtils.reVertexIndexes(janusGraph, janusGraphIndex.getIndexNames());
        } catch (Exception ex) {
            log.error("重建索引异常：", ex);
            throw new RuntimeException(CommonUtils.getRealMessage(ex));
        }
    }

    @ApiOperation(value = "注册索引")
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PostMapping(value = "/registerIndex")
    public void registerIndex(@RequestBody JanusGraphIndex janusGraphIndex) {
        try {
            log.info("注册索引：{}", janusGraphIndex);
            SchemaUtils.registerVertexIndexes(janusGraph, janusGraphIndex.getIndexNames());
        } catch (Exception ex) {
            log.error("注册索引异常：", ex);
            throw new RuntimeException(CommonUtils.getRealMessage(ex));
        }
    }


    private Map<String, String> fetchRelationIndexes(JanusGraphManagement jgm) {
        Iterable<RelationType> relationTypes = jgm.getRelationTypes(RelationType.class);

        return StreamSupport.stream(relationTypes.spliterator(), false)
                .flatMap(r -> StreamSupport.stream(jgm.getRelationIndexes(r).spliterator(), false))
                .collect(Collectors.toMap(e -> e.name(), e -> e.getType().name()));
    }

    private Set<String> fetchVertexIndexes(JanusGraphManagement jgm) {
        Iterable<org.janusgraph.core.schema.JanusGraphIndex> vertexIndexes = jgm.getGraphIndexes(Vertex.class);
        return StreamSupport.stream(vertexIndexes.spliterator(), false)
                .map(org.janusgraph.core.schema.JanusGraphIndex::name)
                .collect(Collectors.toSet());
    }
}
