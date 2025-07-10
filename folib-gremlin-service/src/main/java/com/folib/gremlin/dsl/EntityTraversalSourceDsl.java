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
package com.folib.gremlin.dsl;

import com.folib.data.domain.DomainObject;
import org.apache.tinkerpop.gremlin.process.remote.RemoteConnection;
import org.apache.tinkerpop.gremlin.process.traversal.TraversalStrategies;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.DefaultGraphTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;
import org.apache.tinkerpop.gremlin.process.traversal.step.map.GraphStep;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class to create {@link EntityTraversalDsl} instance.
 *
 * @author veadan
 */
public class EntityTraversalSourceDsl extends GraphTraversalSource
{

    private static final Logger logger = LoggerFactory.getLogger(EntityTraversalSourceDsl.class);

    public EntityTraversalSourceDsl(Graph graph,
                                    TraversalStrategies traversalStrategies)
    {
        super(graph, traversalStrategies);
    }

    public EntityTraversalSourceDsl(Graph graph)
    {
        super(graph);
    }

    public EntityTraversalSourceDsl(RemoteConnection connection)
    {
        super(connection);
    }

    /**
     * Creates {@link EntityTraversalDsl} instance based on specified entity.
     *
     * @param entity
     * @return
     */
    public GraphTraversal<Vertex, Vertex> V(DomainObject entity)
    {
        GraphTraversalSource clone = this.clone();
        clone.getBytecode().addStep(GraphTraversal.Symbols.V);
        GraphTraversal<Vertex, Vertex> traversal = new DefaultGraphTraversal<>(clone);

        Long vertexId = entity.getNativeId();
        if (vertexId != null)
        {
            traversal.asAdmin().addStep(new GraphStep<>(traversal.asAdmin(), Vertex.class, true, vertexId));
        }
        else
        {
            traversal.asAdmin().addStep(new GraphStep<>(traversal.asAdmin(), Vertex.class, true));
        }

        return traversal;
    }
}
