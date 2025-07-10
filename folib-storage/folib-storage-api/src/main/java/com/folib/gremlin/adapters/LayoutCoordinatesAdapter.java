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

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.domain.LayoutCoordinatesEntity;

/**
 * @author veadan
 */
public abstract class LayoutCoordinatesAdapter<C extends LayoutCoordinatesEntity<C, V>, V extends Comparable<V>>
        implements VertexEntityTraversalAdapter<GenericCoordinates>, ArtifactCoodrinatesNodeAdapter
{
    private final String layoutCoorinatesLabel;
    private final Class<C> layoutCoordinatesClass;

    public LayoutCoordinatesAdapter(String label,
                                    Class<C> layoutCoordinatesClass)
    {
        this.layoutCoorinatesLabel = label;
        this.layoutCoordinatesClass = layoutCoordinatesClass;
    }

    @Override
    public String label()
    {
        return layoutCoorinatesLabel;
    }

    @Override
    public Class<C> entityClass()
    {
        return layoutCoordinatesClass;
    }

    @Override
    public EntityTraversal<Vertex, GenericCoordinates> fold()
    {
        return __.<Vertex, Object>project("id", "uuid")
                 .by(__.id())
                 .by(__.enrichPropertyValue("uuid"))
                 .map(this::map);
    }

    private C map(Traverser<Map<String, Object>> t)
    {
        C result = newInstance();
        result.setNativeId(extractObject(Long.class, t.get().get("id")));
        result.setUuid(extractObject(String.class, t.get().get("uuid")));

        return result;
    }

    protected abstract C newInstance();

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(GenericCoordinates entity)
    {
        return new UnfoldEntityTraversal<>(layoutCoorinatesLabel, entity, __.identity());
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        throw new UnsupportedOperationException();
    }

}
