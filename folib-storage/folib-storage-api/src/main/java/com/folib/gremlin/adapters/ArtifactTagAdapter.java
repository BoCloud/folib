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

import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;

import java.util.Map;

import com.folib.gremlin.adapters.UnfoldEntityTraversal;
import com.folib.gremlin.adapters.VertexEntityTraversalAdapter;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.artifact.ArtifactTag;
import com.folib.db.schema.Vertices;
import com.folib.domain.ArtifactTagEntity;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactTagAdapter implements VertexEntityTraversalAdapter<ArtifactTag>
{

    @Override
    public String label()
    {
        return Vertices.ARTIFACT_TAG;
    }

    @Override
    public EntityTraversal<Vertex, ArtifactTag> fold()
    {
        return __.<Vertex, Object>project("id", "uuid")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .map(this::map);
    }

    private ArtifactTag map(Traverser<Map<String, Object>> t)
    {
        ArtifactTagEntity result = new ArtifactTagEntity();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(ArtifactTag entity)
    {
        return new UnfoldEntityTraversal<>(Vertices.ARTIFACT_TAG, entity, __.identity());
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>identity().map(t -> Element.class.cast(t.get()));
    }

}
