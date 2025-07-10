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

import java.util.List;
import java.util.Optional;
import java.util.Set;

import com.folib.data.domain.DomainObject;
import com.folib.gremlin.dsl.EntityTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.Bytecode;
import org.apache.tinkerpop.gremlin.process.traversal.Step;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.TraversalSideEffects;
import org.apache.tinkerpop.gremlin.process.traversal.TraversalStrategies;
import org.apache.tinkerpop.gremlin.process.traversal.TraverserGenerator;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.step.TraversalParent;
import org.apache.tinkerpop.gremlin.process.traversal.traverser.TraverserRequirement;
import org.apache.tinkerpop.gremlin.structure.Graph;

/**
 * Traversal wrapper which is used to unfold entity on graph.
 *
 * @param <S>
 * @param <E>
 *
 * @author veadan
 */
public class UnfoldEntityTraversal<S, E> implements EntityTraversal<S, E>
{

    private final String entityLabel;
    private final DomainObject entity;
    private final EntityTraversal<S, E> target;

    public UnfoldEntityTraversal(String entityLabel,
                                 DomainObject entity,
                                 EntityTraversal<S, E> target)
    {
        this.entityLabel = entityLabel;
        this.entity = entity;
        this.target = target;
    }

    public String getEntityLabel()
    {
        return entityLabel;
    }

    public DomainObject getEntity()
    {
        return entity;
    }

    public EntityTraversal<S, E> getTarget()
    {
        return target;
    }

    @Override
    public Bytecode getBytecode()
    {
        return target.getBytecode();
    }

    @Override
    public List<Step> getSteps()
    {
        return target.getSteps();
    }

    @Override
    public <S2, E2> Traversal.Admin<S2, E2> addStep(int index,
                                                    Step<?, ?> step)
        throws IllegalStateException
    {
        return target.addStep(index, step);
    }

    @Override
    public <S2, E2> Traversal.Admin<S2, E2> removeStep(int index)
        throws IllegalStateException
    {
        return target.removeStep(index);
    }

    @Override
    public void applyStrategies()
        throws IllegalStateException
    {
        target.applyStrategies();
    }

    @Override
    public TraverserGenerator getTraverserGenerator()
    {
        return target.getTraverserGenerator();
    }

    @Override
    public Set<TraverserRequirement> getTraverserRequirements()
    {
        return target.getTraverserRequirements();
    }

    @Override
    public void setSideEffects(TraversalSideEffects sideEffects)
    {
        target.setSideEffects(sideEffects);
    }

    @Override
    public TraversalSideEffects getSideEffects()
    {
        return target.getSideEffects();
    }

    @Override
    public void setStrategies(TraversalStrategies strategies)
    {
        target.setStrategies(strategies);
    }

    @Override
    public TraversalStrategies getStrategies()
    {
        return target.getStrategies();
    }

    @Override
    public void setParent(TraversalParent step)
    {
        target.setParent(step);
    }

    @Override
    public TraversalParent getParent()
    {
        return target.getParent();
    }

    @Override
    public boolean isLocked()
    {
        return target.isLocked();
    }

    /**
     * Lock the traversal and perform any final adjustments to it after strategy application.
     */
    @Override
    public void lock() {

    }

    @Override
    public Optional<Graph> getGraph()
    {
        return target.getGraph();
    }

    @Override
    public void setGraph(Graph graph)
    {
        target.setGraph(graph);
    }

    @Override
    public boolean hasNext()
    {
        return target.hasNext();
    }

    @Override
    public E next()
    {
        return target.next();
    }

    @Override
    public GraphTraversal.Admin<S, E> clone()
    {
        return target.clone();
    }

    public EntityTraversal<S, E> iterate()
    {
        return target.iterate();
    }

}
