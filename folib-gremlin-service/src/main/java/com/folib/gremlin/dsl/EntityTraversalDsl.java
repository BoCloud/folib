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

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.folib.data.domain.DomainObject;
import com.folib.data.domain.EntityHierarchyNode;
import com.folib.gremlin.adapters.UnfoldEntityTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
//import org.apache.tinkerpop.gremlin.process.traversal.dsl.GremlinDsl;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversal;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty.Cardinality;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

/**
 * The application specific Gremlin DSL extension. Provides shortcut methods for the traversals that commonly used.
 *
 * @author veadan
 *
 * @param <S>
 * @param <E>
 * @see <a href="https://tinkerpop.apache.org/docs/current/reference/#gremlin-java-dsl">Domain Specific Languages</a>
 */
public interface EntityTraversalDsl<S, E> extends GraphTraversal.Admin<S, E>
{

    Logger logger = LoggerFactory.getLogger(EntityTraversalDsl.class);

    /**
     * Defines pseudo `null` for properties with no value, needed because Gremlin does not support `null` values.
     */
    Object NULL = new Object()
    {

        @Override
        public String toString()
        {
            return "__null";
        }

    };

    /**
     * Search graph to vertices with specified labels by `uuid`.
     *
     * @param uuid `uuid` property value
     * @param labels vertex labels
     * @return traversal within the search result
     */
    @SuppressWarnings("unchecked")
    default GraphTraversal<S, Vertex> findById(Object uuid,
                                               String... labels)
    {
        GraphTraversal<S, Vertex> result = (GraphTraversal<S, Vertex>) has(labels[0], "uuid", uuid);
        for (String label : Arrays.copyOfRange(labels, 1, labels.length))
        {
            result = result.fold().choose(Collection::isEmpty, __.V().has(label, "uuid", uuid), __.unfold());
        }

        return result;
    }

    /**
     * Get the property value for current traversal element, or {@link EntityTraversalDsl#NULL} if no such property.
     *
     * @param propertyName name of the property
     * @return property value traversal
     */
    @SuppressWarnings("unchecked")
    default Traversal<S, Object> enrichPropertyValue(String propertyName)
    {
        return coalesce(__.properties(propertyName).value(), __.<Object>constant(NULL));
    }

    /**
     * Get the collection of property values for current traversal element, or {@link EntityTraversalDsl#NULL} if no such property.
     *
     * @param propertyName name of the property
     * @return property values collection traversal
     */
    @SuppressWarnings("unchecked")
    default Traversal<S, Object> enrichPropertyValues(String propertyName)
    {
        return coalesce(__.propertyMap(propertyName).map(t -> t.get().get(propertyName)), __.<Object>constant(NULL));
    }

    /**
     * Traverse to the expected entity value, or {@link EntityTraversalDsl#NULL} if no such entity.
     *
     * @param <S2> entity type
     * @param enrichObjectTraversal entity traversal
     * @return entity value traversal
     */
    default <S2> Traversal<S, Object> mapToObject(Traversal<S2, Object> enrichObjectTraversal)
    {
        return fold().choose(t -> t.isEmpty(),
                             __.<Object>constant(NULL),
                             __.<Edge>unfold().map(enrichObjectTraversal));
    }

    /**
     * Traverse the graph to the specified entity.
     *
     * @param entity entity value
     * @return entity vertex traversal
     */
    default GraphTraversal<S, Vertex> V(DomainObject entity)
    {
        if (entity instanceof EntityHierarchyNode)
        {
            EntityHierarchyNode entityHierarchyNode = (EntityHierarchyNode) entity;
            while (entityHierarchyNode.getHierarchyParent() != null)
            {
                entityHierarchyNode = entityHierarchyNode.getHierarchyParent();
            }
            entity = (DomainObject) entityHierarchyNode;
        }

        Long nativeId = entity.getNativeId();
        if (nativeId != null)
        {
            return ((EntityTraversal<S, Vertex>) V(nativeId)).info("Attached");
        }

        return V();
    }

    /**
     * Checks if the given entity exists, and adds or updates the entity as appropriate.
     *
     * @param <S2>
     * @param uuid entity `uuid`
     * @param unfoldTraversal traversal to unfold the entity on graph
     * @return entity vertex traversal
     */
    default <S2> Traversal<S, Vertex> saveV(Object uuid,
                                            UnfoldEntityTraversal<S2, Vertex> unfoldTraversal)
    {
        uuid = Optional.ofNullable(uuid)
                       .orElse(NULL);
        DomainObject entity = unfoldTraversal.getEntity();
        String label = unfoldTraversal.getEntityLabel();

        return hasLabel(label).has("uuid", uuid)
                              .fold()
                              .choose(Collection::isEmpty,
                                      __.addV(uuid, unfoldTraversal),
                                      __.<Vertex>unfold()
                                        .sideEffect(entity::applyUnfold)
                                        .sideEffect(EntityTraversalUtils::fetched))
                              .map(unfoldTraversal);
    }

    /**
     * Adds entity with specified `uuid`
     *
     * @param <S2>
     * @param uuid entity `uuid`
     * @param unfoldTraversal traversal to unfold the entity on graph
     * @return entity vertex traversal
     */
    default <S2> Traversal<S, Vertex> addV(Object uuid,
                                           UnfoldEntityTraversal<S2, Vertex> unfoldTraversal)
    {
        String label = unfoldTraversal.getEntityLabel();
        DomainObject entity = unfoldTraversal.getEntity();

        return addV(label).property("uuid",
                                    Optional.of(uuid)
                                            .filter(x -> !NULL.equals(x))
                                            .orElse(UUID.randomUUID().toString()))
                          .property("created", System.currentTimeMillis())
                          .sideEffect(entity::applyUnfold)
                          .sideEffect(EntityTraversalUtils::created);
    }

    /**
     * Logs traverser value labeled with action.
     *
     * @param <E2>
     * @param action the action label
     * @return current traversal
     */
    @SuppressWarnings("unchecked")
    default <E2> Traversal<S, E2> info(String action)
    {
        return (Traversal<S, E2>) sideEffect(t -> EntityTraversalUtils.info(action, t));
    }

    /**
     * Logs traverser value labeled with action.
     *
     * @param <E2>
     * @param action the action label
     * @return current traversal
     */
    @SuppressWarnings("unchecked")
    default <E2> Traversal<S, E2> debug(String action)
    {
        return (Traversal<S, E2>) sideEffect(t -> EntityTraversalUtils.debug(action, t));
    }

    /**
     * Sets the property value with {@link Cardinality#set}
     *
     * @param <E2>
     * @param key property key
     * @param values property value
     * @return current traversal
     */
    default <E2> GraphTraversal<S, E2> property(final String key,
                                                final Set<String> values)
    {

        if (CollectionUtils.isEmpty(values))
        {
            return (GraphTraversal<S, E2>) identity();
        }

        GraphTraversal<S, E2> t = (GraphTraversal<S, E2>) property(Cardinality.set, key, "");
        for (String value : values)
        {
            t.property(Cardinality.set, key, value);
        }

        return t;
    }
}
