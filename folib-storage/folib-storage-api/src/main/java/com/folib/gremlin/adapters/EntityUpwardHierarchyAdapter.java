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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.__;
import org.apache.commons.collections4.iterators.FilterIterator;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.data.domain.DomainObject;
import com.folib.data.domain.EntityHierarchyNode;
import com.folib.db.schema.Edges;

/**
 * Adapts hierarchical entities to the Graph structure. 
 * 
 * @author veadan
 */
public abstract class EntityUpwardHierarchyAdapter<E extends DomainObject & EntityHierarchyNode,
                                                   A extends EntityUpwardHierarchyNodeAdapter<E>>
        implements VertexEntityTraversalAdapter<E>
{

    private final int maxHierarchyDepth;

    /**
     * EntityUpwardHierarchyNodeAdapters mapped by Vertex label and sorted in
     * descending order by entity class hierarchy.
     */
    protected final Deque<A> adaptersHierarchy;

    public EntityUpwardHierarchyAdapter(Set<A> artifactArapters,
                                        int maxHierarchyDepth)
    {
        this.maxHierarchyDepth = maxHierarchyDepth;
        this.adaptersHierarchy = artifactArapters.stream()
                                                 .sorted((a1,
                                                          a2) -> a1.entityClass().isAssignableFrom(a2.entityClass()) ? 1 : -1)
                                                 .collect(Collectors.toCollection(LinkedList::new));
    }

    @Override
    public String label()
    {
        return getRootAdapter().label();
    }

    /**
     * Gets the entity hierarchy root class adapter.
     * 
     * @return
     */
    protected EntityUpwardHierarchyNodeAdapter<E> getRootAdapter()
    {
        return adaptersHierarchy.descendingIterator().next();
    }

    protected Deque<A> getAdaptersHierarchy()
    {
        return adaptersHierarchy;
    }

    @Override
    public EntityTraversal<Vertex, E> fold()
    {
        return fold(Optional.empty());
    }
    
    public EntityTraversal<Vertex, E> fold(Optional<Class<? extends E>> targetClass)
    {
        //Fold current vertex starting from root node adapter 
        Optional<EntityTraversal<Vertex, E>> upwardTraversal = fold(targetClass,
                                                                    Collections.singleton((A) getRootAdapter()).iterator(),
                                                                    maxHierarchyDepth);
        return __.map(upwardTraversal.get());
    }

    /**
     * Folds vertex into current hierarchy level entity. Recursively goes
     * through the all available adapters to find one that could be applied for
     * current vertex label.
     * 
     * @param iterator
     *            available adapters iterator
     * @param level
     *            current hierarchy level
     * @return hierarchy traversal subtree, or empty if there is no next adapter
     */
    private Optional<EntityTraversal<Vertex, E>> fold(Optional<Class<? extends E>> targetClass,
                                                      Iterator<A> iterator,
                                                      int level)
    {
        Iterator<A> filteredIterator = targetClass.map(c -> (Iterator<A>) new FilterIterator<>(iterator,
                (adapter) -> adapter.entityClass().isAssignableFrom(c))).orElse(iterator);
        
        if (!filteredIterator.hasNext())
        {
            return Optional.empty();
        }

        A currentAdapter = filteredIterator.next();
        
        EntityTraversal<Vertex, E> result = fold(targetClass, filteredIterator, level).map(nextTraversal -> {
            // If there is upward node then try to fold node with hierarchy
            // subtree
            return foldNode(targetClass, level, currentAdapter, Optional.of(nextTraversal));
        }).orElseGet(() -> {// If there is no upward node then try to fold only
                            // current node using current adapter
            return foldNode(targetClass, level, currentAdapter, Optional.empty());
        });

        return Optional.of(result);
    }

    /**
     * Folds current node using provided adapter and then moves traversal up to the
     * next hierarchy vertex by `extends` edge and in breadth by other possible adapters.
     * 
     * @param level
     *            current hierarchy level
     * @param currentNodeAdapter
     *            current adapter
     * @param nextTraversal
     *            the traversal that should be applied if current adapter label
     *            not matches the current vertex label
     * @return current node traversal with hierarchy subtree above the current level
     */
    private EntityTraversal<Vertex, E> foldNode(Optional<Class<? extends E>> targetClass,
                                                int level,
                                                A currentNodeAdapter,
                                                Optional<EntityTraversal<Vertex, E>> nextTraversal)
    {
        String nodeLabel = currentNodeAdapter.label();
        EntityTraversal<Vertex, E> currentNodeTraversal = currentNodeAdapter.fold();
        EntityTraversal<Vertex, E> topTraversal = nextTraversal.map(t -> chainNext(nodeLabel, __.map(currentNodeTraversal), t))
                                                               .orElse(__.map(currentNodeTraversal));
        if (level == 0)
        {
            return topTraversal;
        }

        Iterator<A> childIterator = getChildIterator();
        return fold(targetClass, childIterator, level - 1).map(this::childTraversal)
                                             .map(childTraversal -> nextTraversal.map(t -> chainNext(nodeLabel,
                                                                                                     nodeTraversal(currentNodeTraversal,
                                                                                                                   childTraversal),
                                                                                                     t))
                                                                                 .orElse(nodeTraversal(currentNodeTraversal,
                                                                                                       childTraversal)))
                                             .orElse(topTraversal);
    }

    /**
     * Makes the traversal branches by the node label. 
     * 
     * @param nodeLabel current node label
     * @param nodeTraversal current traversal
     * @param nextTraversal next available node traversal
     * @return branched traversals from current and next node adapters
     */
    private EntityTraversal<Vertex, E> chainNext(String nodeLabel,
                                                 EntityTraversal<Vertex, E> nodeTraversal,
                                                 EntityTraversal<Vertex, E> nextTraversal)
    {
        return __.<Vertex, E>choose(t -> nodeLabel.equals(t.label()),
                                    __.map(nodeTraversal),
                                    nextTraversal);
    }

    /**
     * Moves traversal up by the hierarchy
     * 
     * @param childTraversal child hierarchy traversal
     * @return traversal moved to next hierarchy vertex
     */
    private EntityTraversal<Vertex, E> childTraversal(EntityTraversal<Vertex, E> childTraversal)
    {
        return __.inE(Edges.EXTENDS)
                 .outV()
                 .map(childTraversal);
    }

    /**
     * Directly folds the current vertex into node entity with it's child entity 
     * 
     * @param nodeTraversal current node traversal
     * @param childTraversal child node traversal
     * @return traversal with node entity
     */
    private EntityTraversal<Vertex, E> nodeTraversal(EntityTraversal<Vertex, E> nodeTraversal,
                                                     EntityTraversal<Vertex, E> childTraversal)
    {
        return __.<Vertex, E>project("parent", "child")
                 .by(nodeTraversal)
                 .by(childTraversal)
                 .sideEffect(this::parentWithChild)
                 .<E>select("child");
    }

    protected Iterator<A> getChildIterator()
    {
        // Get child iterator
        Iterator<A> childIterator = getAdaptersHierarchy().descendingIterator();
        // Skip root adapter
        childIterator.next();

        return childIterator;
    }

    private void parentWithChild(Traverser<Map<String, E>> t)
    {
        EntityHierarchyNode parent = extractObject(EntityHierarchyNode.class, t.get().get("parent"));
        EntityHierarchyNode child = extractObject(EntityHierarchyNode.class, t.get().get("child"));

        parent.setHierarchyChild(child);
        child.setHierarchyParent(parent);
    }

    @Override
    public UnfoldEntityTraversal<Vertex, Vertex> unfold(E entity)
    {

        List<E> hierarchy = new ArrayList<>();
        hierarchy.add(entity);
        while (entity.getHierarchyParent() != null)
        {
            hierarchy.add(0, entity = (E) entity.getHierarchyParent());
        }
        EntityUpwardHierarchyNodeAdapter<E> rootAdapter = getRootAdapter();
        Class<? extends E> rootEntityType = rootAdapter.entityClass();
        E rootEntity = hierarchy.iterator().next();
        if (!rootEntityType.isAssignableFrom(rootEntity.getClass()))
        {
            throw new IllegalArgumentException(
                    String.format("Invalid hierarchy root type [%s], should be assignable tp [%s].",
                                  rootEntity.getClass(),
                                  rootEntityType));
        }

        EntityTraversal<Vertex, Vertex> result = null;
        for (E hierarchyNode : hierarchy)
        {
            for (A hierarchyNodeAdapter : adaptersHierarchy)
            {
                if (!hierarchyNodeAdapter.entityClass().isAssignableFrom(hierarchyNode.getClass()))
                {
                    continue;
                }
                else if (result == null)
                {
                    result = hierarchyNodeAdapter.unfold(hierarchyNode);
                }
                else
                {
                    result = result.optional(__.inE(Edges.EXTENDS)
                                               .outV())
                                   .choose(__.hasLabel(hierarchyNodeAdapter.label()),
                                           __.saveV(hierarchyNode.getUuid(), hierarchyNodeAdapter.unfold(hierarchyNode)),
                                           __.addE(Edges.EXTENDS)
                                             .from(__.addV(hierarchyNode.getUuid(),
                                                           hierarchyNodeAdapter.unfold(hierarchyNode)))
                                             .outV());
                }
                break;
            }
        }
        // Move back to root vertex
        for (int i = 0; i < hierarchy.size() - 1; i++)
        {
            result = result.outE(Edges.EXTENDS).inV();
        }

        return new UnfoldEntityTraversal<>(rootAdapter.label(), entity, result);
    }

}
