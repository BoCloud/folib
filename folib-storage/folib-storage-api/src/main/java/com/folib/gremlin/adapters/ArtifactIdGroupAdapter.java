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

import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;
import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;

import java.util.*;

import jakarta.inject.Inject;

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.artifact.ArtifactTag;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.db.schema.Edges;
import com.folib.db.schema.Properties;
import com.folib.db.schema.Vertices;
import com.folib.domain.Artifact;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.gremlin.dsl.EntityTraversalUtils;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactIdGroupAdapter implements VertexEntityTraversalAdapter<ArtifactIdGroup>
{

    @Inject
    private ArtifactAdapter artifactAdapter;

    @Override
    public String label()
    {
        return Vertices.ARTIFACT_ID_GROUP;
    }

    public EntityTraversal<Vertex, ArtifactIdGroup> fold(Optional<Class<? extends GenericCoordinates>> layoutArtifactCoordinatesClass,
                                                         Optional<ArtifactTag> optionalTag)
    {
        EntityTraversal<Vertex, Vertex> artifactsTraversal = optionalTag.map(ArtifactTag::getName)
                                                                        .map(tagName -> __.outE(Edges.ARTIFACT_GROUP_HAS_TAGGED_ARTIFACTS)
                                                                                          .has(Properties.TAG_NAME, tagName)
                                                                                          .inV())
                                                                        .orElse(__.outE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS)
                                                                                  .inV());

        return __.<Vertex, Object>project("id", "uuid", "storageId", "repositoryId", "name", "artifacts")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .by(__.enrichPropertyValue("storageId"))
                 .by(__.enrichPropertyValue("repositoryId"))
                 .by(__.enrichPropertyValue("name"))
                 .by(artifactsTraversal.dedup()
                                       .map(artifactAdapter.fold(layoutArtifactCoordinatesClass))
                                       .map(EntityTraversalUtils::castToObject)
                                       .fold())
                 .map(this::map);
    }

    public EntityTraversal<Vertex, ArtifactIdGroup> artifactIdGroupFold()
    {
        return __.<Vertex, Object>project("id", "uuid", "storageId", "repositoryId", "name", "metadata")
                .by(__.id())
                .by(__.enrichPropertyValue("uuid"))
                .by(__.enrichPropertyValue("storageId"))
                .by(__.enrichPropertyValue("repositoryId"))
                .by(__.enrichPropertyValue("name"))
                .by(__.enrichPropertyValue("metadata"))
                .map(this::map);
    }

    @Override
    public EntityTraversal<Vertex, ArtifactIdGroup> fold()
    {
        return fold(Optional.of(GenericCoordinates.class), Optional.empty());
    }

    private ArtifactIdGroup map(Traverser<Map<String, Object>> t)
    {
        ArtifactIdGroupEntity result = new ArtifactIdGroupEntity(extractObject(String.class, t.get().get("storageId")),
                                                                 extractObject(String.class, t.get().get("repositoryId")),
                                                                 extractObject(String.class, t.get().get("name")));
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));
        result.setMetadata(extractObject(String.class, t.get().get("metadata")));
        Object artifact = t.get().get("artifacts");
        if (Objects.nonNull(artifact)) {
            Collection<Artifact> artifactCollection = (Collection<Artifact>) t.get().get("artifacts");
            artifactCollection.stream().forEach(result::addArtifact);
        }
        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(ArtifactIdGroup entity)
    {
        String storedArtifactIdGroup = Vertices.ARTIFACT_ID_GROUP + ":" + UUID.randomUUID();
        EntityTraversal<Vertex, Vertex> connectArtifacstTraversal = __.identity();
        if (CollectionUtils.isNotEmpty(entity.getArtifacts())) {
            for (Artifact artifact : entity.getArtifacts())
            {
                connectArtifacstTraversal = connectArtifacstTraversal.V(artifact)
                        .saveV(artifact.getUuid(),
                                artifactAdapter.unfold(artifact));

                connectArtifacstTraversal.choose(__.inE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS),
                        __.identity(),
                        __.addE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS)
                                .from(__.<Vertex, Vertex>select(storedArtifactIdGroup).unfold())
                                .inV());

                connectArtifacstTraversal = connectArtifacstTraversal.sideEffect(__.inE(Edges.ARTIFACT_GROUP_HAS_TAGGED_ARTIFACTS).drop());
                for (ArtifactTag artifactTag : artifact.getTagSet())
                {
                    connectArtifacstTraversal = connectArtifacstTraversal.addE(Edges.ARTIFACT_GROUP_HAS_TAGGED_ARTIFACTS)
                            .from(__.<Vertex, Vertex>select(storedArtifactIdGroup).unfold())
                            .property(Properties.TAG_NAME, artifactTag.getName())
                            .inV();

                }
                connectArtifacstTraversal = connectArtifacstTraversal.inE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS).outV();
            }
        }
        EntityTraversal<Vertex, Vertex> unfoldTraversal = __.<Vertex, Vertex>map(unfoldArtifactGroup(entity))
                                                            .store(storedArtifactIdGroup)
                                                            .flatMap(connectArtifacstTraversal)
                                                            .fold().map(t -> t.get().iterator().next());

        return new UnfoldEntityTraversal<>(Vertices.ARTIFACT_ID_GROUP, entity, unfoldTraversal);
    }

    private EntityTraversal<Vertex, Vertex> unfoldArtifactGroup(ArtifactIdGroup entity)
    {
        EntityTraversal<Vertex, Vertex> t = __.identity();
        // Skip update as ArtifactIdGroup assumed to be immutable
        if (entity.getNativeId() != null)
        {
            if (entity.getMetadata() != null)
            {
                t = t.property(single, "metadata", entity.getMetadata());
            }
            return t;
        }

        if (entity.getStorageId() != null)
        {
            t = t.property(single, "storageId", entity.getStorageId());
        }
        if (entity.getRepositoryId() != null)
        {
            t = t.property(single, "repositoryId", entity.getRepositoryId());
        }
        if (entity.getName() != null)
        {
            t = t.property(single, "name", entity.getName());
        }
        if (entity.getMetadata() != null)
        {
            t = t.property(single, "metadata", entity.getMetadata());
        }

        return t;
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>aggregate("x")
                 .optional(__.outE(Edges.ARTIFACT_GROUP_HAS_ARTIFACTS)
                             .inV()
                             .flatMap(artifactAdapter.cascade()))
                 .select("x")
                 .unfold();
    }

}
