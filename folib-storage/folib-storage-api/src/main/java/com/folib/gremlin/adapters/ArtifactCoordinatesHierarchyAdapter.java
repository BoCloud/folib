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

import java.util.Set;

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.artifact.coordinates.GenericCoordinates;
import com.folib.db.schema.Edges;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactCoordinatesHierarchyAdapter
        extends EntityUpwardHierarchyAdapter<GenericCoordinates, ArtifactCoodrinatesNodeAdapter>
{

    public ArtifactCoordinatesHierarchyAdapter(Set<ArtifactCoodrinatesNodeAdapter> artifactAdapters)
    {
        super(artifactAdapters, 1);
    }

    @Override
    public EntityTraversal<Vertex, Element> cascade()
    {
        return __.<Vertex>aggregate("x")
                 .inE(Edges.EXTENDS)
                 .outV()
                 .aggregate("x")
                 .select("x")
                 .unfold();
    }

}
