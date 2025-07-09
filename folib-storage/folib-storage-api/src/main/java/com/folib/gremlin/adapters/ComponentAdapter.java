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

import com.folib.gremlin.adapters.UnfoldEntityTraversal;
import com.folib.gremlin.adapters.VertexEntityTraversalAdapter;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.domain.Component;
import com.folib.domain.ComponentEntity;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import static cn.hutool.core.convert.Convert.toLocalDateTime;
import static com.folib.gremlin.dsl.EntityTraversalUtils.*;
import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;

/**
 * @author veadan
 */
@org.springframework.stereotype.Component
public class ComponentAdapter implements VertexEntityTraversalAdapter<Component> {

    @Override
    public String label() {
        return Vertices.COMPONENT;
    }

    @Override
    public EntityTraversal<Vertex, Component> fold() {
        return __.<Vertex, Object>project("id", "uuid", "created", "lastUpdated", "name",
                "fileName", "version", "groupId", "description", "purl", "url", "cpe", "md5sum", "sha1sum", "sha256sum",
                "license", "vulnerabilities", "vulnerabilitiesCount", "criticalVulnerabilitiesCount", "highVulnerabilitiesCount",
                "mediumVulnerabilitiesCount", "lowVulnerabilitiesCount", "suppressedVulnerabilitiesCount")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("created"))
                .by(__.enrichPropertyValue("lastUpdated"))
                .by(__.enrichPropertyValue("name"))
                .by(__.enrichPropertyValue("fileName"))
                .by(__.enrichPropertyValue("version"))
                .by(__.enrichPropertyValue("groupId"))
                .by(__.enrichPropertyValue("description"))
                .by(__.enrichPropertyValue("purl"))
                .by(__.enrichPropertyValue("url"))
                .by(__.enrichPropertyValue("cpe"))
                .by(__.enrichPropertyValue("md5sum"))
                .by(__.enrichPropertyValue("sha1sum"))
                .by(__.enrichPropertyValue("sha256sum"))
                .by(__.enrichPropertyValues("license"))
                .by(__.enrichPropertyValues("vulnerabilities"))
                .by(__.enrichPropertyValue("vulnerabilitiesCount"))
                .by(__.enrichPropertyValue("criticalVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("highVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("mediumVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("lowVulnerabilitiesCount"))
                .by(__.enrichPropertyValue("suppressedVulnerabilitiesCount"))
                .map(this::map);
    }

    private Component map(Traverser<Map<String, Object>> t) {
        ComponentEntity result = new ComponentEntity();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));
        result.setCreated(toLocalDateTime(extractObject(Long.class, t.get().get("created"))));
        result.setLastUpdated(toLocalDateTime(extractObject(Long.class, t.get().get("lastUpdated"))));
        result.setName(extractObject(String.class, t.get().get("name")));
        result.setFileName(extractObject(String.class, t.get().get("fileName")));
        result.setVersion(extractObject(String.class, t.get().get("version")));
        result.setGroupId(extractObject(String.class, t.get().get("groupId")));
        result.setDescription(extractObject(String.class, t.get().get("description")));
        result.setPurl(extractObject(String.class, t.get().get("purl")));
        result.setUrl(extractObject(String.class, t.get().get("url")));
        result.setCpe(extractObject(String.class, t.get().get("cpe")));
        result.setMd5sum(extractObject(String.class, t.get().get("md5sum")));
        result.setSha1sum(extractObject(String.class, t.get().get("sha1sum")));
        result.setSha256sum(extractObject(String.class, t.get().get("sha256sum")));
        result.setLicense(extractPropertyList(String.class, t.get().get("license")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toCollection(LinkedHashSet::new)));
        result.setVulnerabilities(extractPropertyList(String.class, t.get().get("vulnerabilities")).stream()
                .filter(e -> !e.trim().isBlank())
                .collect(Collectors.toCollection(LinkedHashSet::new)));
        result.setVulnerabilitiesCount(extractObject(Integer.class, t.get().get("vulnerabilitiesCount")));
        result.setCriticalVulnerabilitiesCount(extractObject(Integer.class, t.get().get("criticalVulnerabilitiesCount")));
        result.setHighVulnerabilitiesCount(extractObject(Integer.class, t.get().get("highVulnerabilitiesCount")));
        result.setMediumVulnerabilitiesCount(extractObject(Integer.class, t.get().get("mediumVulnerabilitiesCount")));
        result.setLowVulnerabilitiesCount(extractObject(Integer.class, t.get().get("lowVulnerabilitiesCount")));
        result.setSuppressedVulnerabilitiesCount(extractObject(Integer.class, t.get().get("suppressedVulnerabilitiesCount")));
        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(Component entity) {
        EntityTraversal<Vertex, Vertex> unfoldTraversal = __.identity();
        String storedComponentId = Vertices.COMPONENT + ":" + UUID.randomUUID().toString();
        unfoldTraversal = unfoldTraversal.map(unfoldComponent(entity))
                .store(storedComponentId);
        String drop = "drop";
        if (CollectionUtils.isNotEmpty(entity.getVulnerabilities())) {
            unfoldTraversal = unfoldTraversal.sideEffect(__.outE(Edges.COMPONENT_HAS_VULNERABILITIES).drop());
            if (entity.getVulnerabilities().stream().noneMatch(drop::equalsIgnoreCase)) {
                for (String vulnerability : entity.getVulnerabilities()) {
                    EntityTraversal<Object, Vertex> componentEntityTraversal = __.V().hasLabel(Vertices.VULNERABILITY)
                            .has(Properties.UUID, vulnerability);
                    componentEntityTraversal = componentEntityTraversal.addE(Edges.COMPONENT_HAS_VULNERABILITIES)
                            .from(__.<Vertex, Vertex>select(storedComponentId).unfold())
                            .property(Properties.UUID, entity.getUuid()).inV();
                    unfoldTraversal = unfoldTraversal.sideEffect(componentEntityTraversal);
                }
            }
        }
        return new UnfoldEntityTraversal<>(Vertices.COMPONENT, entity, unfoldTraversal);
    }

    private EntityTraversal<Vertex, Vertex> unfoldComponent(Component entity) {
        EntityTraversal<Vertex, Vertex> t = __.identity();
        if (Objects.nonNull(entity.getCreated())) {
            t = t.property(single, "created", toLong(entity.getCreated()));
        }
        if (Objects.nonNull(entity.getLastUpdated())) {
            t = t.property(single, "lastUpdated", toLong(entity.getLastUpdated()));
        }
        if (StringUtils.isNotBlank(entity.getName())) {
            t = t.property(single, "name", entity.getName());
        }
        if (StringUtils.isNotBlank(entity.getFileName())) {
            t = t.property(single, "fileName", entity.getFileName());
        }
        if (StringUtils.isNotBlank(entity.getVersion())) {
            t = t.property(single, "version", entity.getVersion());
        }
        if (StringUtils.isNotBlank(entity.getGroupId())) {
            t = t.property(single, "groupId", entity.getGroupId());
        }
        if (StringUtils.isNotBlank(entity.getDescription())) {
            t = t.property(single, "description", entity.getDescription());
        }
        if (StringUtils.isNotBlank(entity.getPurl())) {
            t = t.property(single, "purl", entity.getPurl());
        }
        if (StringUtils.isNotBlank(entity.getUrl())) {
            t = t.property(single, "url", entity.getUrl());
        }
        if (StringUtils.isNotBlank(entity.getCpe())) {
            t = t.property(single, "cpe", entity.getCpe());
        }
        if (StringUtils.isNotBlank(entity.getMd5sum())) {
            t = t.property(single, "md5sum", entity.getMd5sum());
        }
        if (StringUtils.isNotBlank(entity.getSha1sum())) {
            t = t.property(single, "sha1sum", entity.getSha1sum());
        }
        if (StringUtils.isNotBlank(entity.getSha256sum())) {
            t = t.property(single, "sha256sum", entity.getSha256sum());
        }
        if (CollectionUtils.isNotEmpty(entity.getLicense())) {
            t = t.sideEffect(__.properties("license").drop());
            t = t.property("license", entity.getLicense());
        }
        String drop = "drop";
        if (CollectionUtils.isNotEmpty(entity.getVulnerabilities())) {
            t = t.sideEffect(__.properties("vulnerabilities").drop());
            if (entity.getVulnerabilities().stream().noneMatch(drop::equalsIgnoreCase)) {
                t = t.property("vulnerabilities", entity.getVulnerabilities());
            }
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
        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade() {
        return __.<Vertex>identity().map(t -> Element.class.cast(t.get()));
    }

}
