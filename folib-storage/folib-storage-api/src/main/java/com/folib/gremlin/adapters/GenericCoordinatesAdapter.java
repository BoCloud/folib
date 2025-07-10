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


import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.db.schema.Vertices;
import com.folib.domain.GenericCoordinatesEntity;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Map.Entry;

import static com.folib.gremlin.dsl.EntityTraversalUtils.extractObject;
import static com.folib.gremlin.dsl.EntityTraversalUtils.extractPropertyList;
import static org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality.single;

/**
 * @author xuxinping
 */
@Component
public class GenericCoordinatesAdapter
        implements VertexEntityTraversalAdapter<GenericCoordinates>, ArtifactCoodrinatesNodeAdapter
{

    @Override
    public String label()
    {
        return Vertices.GENERIC_COORDINATES;
    }

    @Override
    public Class<? extends GenericCoordinates> entityClass()
    {
        return GenericCoordinates.class;
    }

    @Override
    public EntityTraversal<Vertex, GenericCoordinates> fold()
    {
        return __.<Vertex, Object>project("id", "uuid", "version", "coordinates")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .by(__.enrichPropertyValue("version"))
                 .by(__.propertyMap())
                 .map(this::map);
    }

    private GenericCoordinates map(Traverser<Map<String, Object>> t)
    {
        GenericCoordinatesEntity result = new GenericCoordinatesEntity();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));
        result.setVersion(extractObject(String.class, t.get().get("version")));

        Map<String, Object> coordinates = (Map<String, Object>) t.get().get("coordinates");
        coordinates.remove("uuid");
        coordinates.remove("version");
        coordinates.remove("created");
        coordinates.entrySet()
                   .stream()
                   .forEach(e -> result.setCoordinate(e.getKey().replace("coordinates.", ""),
                                                      extractPropertyList(String.class, e.getValue()).iterator().next()));

        return result;
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(GenericCoordinates entity)
    {
        EntityTraversal<Vertex, Vertex> t = __.<Vertex>identity();

        if (entity.getVersion() != null)
        {
            t = t.property(single, "version", entity.getVersion());
        }

        for (Entry<String, String> coordinateEntry : entity.getCoordinates().entrySet())
        {
            if (coordinateEntry.getValue() == null)
            {
                continue;
            }
            t = t.property(single, "coordinates." + coordinateEntry.getKey(), coordinateEntry.getValue());
        }

        return new UnfoldEntityTraversal<>(Vertices.GENERIC_COORDINATES, entity, t);
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        throw new UnsupportedOperationException();
    }

}
