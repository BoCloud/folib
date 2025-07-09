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
import com.folib.gremlin.adapters.UnfoldEntityTraversal;
import java.lang.Comparable;
import java.lang.Double;
import java.lang.Integer;
import java.lang.Long;
import java.lang.Number;
import java.lang.Object;
import java.lang.Override;
import java.lang.String;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;

import org.apache.tinkerpop.gremlin.process.computer.VertexProgram;
import org.apache.tinkerpop.gremlin.process.traversal.Order;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.process.traversal.Path;
import org.apache.tinkerpop.gremlin.process.traversal.Pop;
import org.apache.tinkerpop.gremlin.process.traversal.Scope;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.Traverser;
import org.apache.tinkerpop.gremlin.process.traversal.step.util.Tree;
import org.apache.tinkerpop.gremlin.process.traversal.traverser.util.TraverserSet;
import org.apache.tinkerpop.gremlin.process.traversal.util.TraversalMetrics;
import org.apache.tinkerpop.gremlin.structure.Column;
import org.apache.tinkerpop.gremlin.structure.Direction;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Property;
import org.apache.tinkerpop.gremlin.structure.T;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.VertexProperty;

public interface EntityTraversal<S, E> extends EntityTraversalDsl<S, E> {
    @Override
    default EntityTraversal<S, Vertex> findById(Object uuid, String... labels) {
        return (EntityTraversal) EntityTraversalDsl.super.findById(uuid,labels);
    }

    @Override
    default EntityTraversal<S, Object> enrichPropertyValue(String propertyName) {
        return (EntityTraversal) EntityTraversalDsl.super.enrichPropertyValue(propertyName);
    }

    @Override
    default EntityTraversal<S, Object> enrichPropertyValues(String propertyName) {
        return (EntityTraversal) EntityTraversalDsl.super.enrichPropertyValues(propertyName);
    }

    @Override
    default <S2> EntityTraversal<S, Object> mapToObject(Traversal<S2, Object> enrichObjectTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.mapToObject(enrichObjectTraversal);
    }

    @Override
    default EntityTraversal<S, Vertex> V(DomainObject entity) {
        return (EntityTraversal) EntityTraversalDsl.super.V(entity);
    }

    @Override
    default <S2> EntityTraversal<S, Vertex> saveV(Object uuid,
                                                  UnfoldEntityTraversal<S2, Vertex> unfoldTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.saveV(uuid,unfoldTraversal);
    }

    @Override
    default <S2> EntityTraversal<S, Vertex> addV(Object uuid,
                                                 UnfoldEntityTraversal<S2, Vertex> unfoldTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.addV(uuid,unfoldTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, E2> info(String action) {
        return (EntityTraversal) EntityTraversalDsl.super.info(action);
    }

    @Override
    default <E2> EntityTraversal<S, E2> debug(String action) {
        return (EntityTraversal) EntityTraversalDsl.super.debug(action);
    }

    @Override
    default <E2> EntityTraversal<S, E2> property(final String key, final Set<String> values) {
        return (EntityTraversal) EntityTraversalDsl.super.property(key,values);
    }

    @Override
    default <E2> EntityTraversal<S, E2> map(Function<Traverser<E>, E2> function) {
        return (EntityTraversal) EntityTraversalDsl.super.map(function);
    }

    @Override
    default <E2> EntityTraversal<S, E2> map(Traversal<?, E2> mapTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.map(mapTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, E2> flatMap(Function<Traverser<E>, Iterator<E2>> function) {
        return (EntityTraversal) EntityTraversalDsl.super.flatMap(function);
    }

    @Override
    default <E2> EntityTraversal<S, E2> flatMap(Traversal<?, E2> flatMapTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.flatMap(flatMapTraversal);
    }

    @Override
    default EntityTraversal<S, Object> id() {
        return (EntityTraversal) EntityTraversalDsl.super.id();
    }

    @Override
    default EntityTraversal<S, String> label() {
        return (EntityTraversal) EntityTraversalDsl.super.label();
    }

    @Override
    default EntityTraversal<S, E> identity() {
        return (EntityTraversal) EntityTraversalDsl.super.identity();
    }

    @Override
    default <E2> EntityTraversal<S, E2> constant(E2 e) {
        return (EntityTraversal) EntityTraversalDsl.super.constant(e);
    }

    @Override
    default EntityTraversal<S, Vertex> V(Object... vertexIdsOrElements) {
        return (EntityTraversal) EntityTraversalDsl.super.V(vertexIdsOrElements);
    }

    @Override
    default EntityTraversal<S, Vertex> to(Direction direction, String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.to(direction,edgeLabels);
    }

    @Override
    default EntityTraversal<S, Vertex> out(String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.out(edgeLabels);
    }

    @Override
    default EntityTraversal<S, Vertex> in(String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.in(edgeLabels);
    }

    @Override
    default EntityTraversal<S, Vertex> both(String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.both(edgeLabels);
    }

    @Override
    default EntityTraversal<S, Edge> toE(Direction direction, String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.toE(direction,edgeLabels);
    }

    @Override
    default EntityTraversal<S, Edge> outE(String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.outE(edgeLabels);
    }

    @Override
    default EntityTraversal<S, Edge> inE(String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.inE(edgeLabels);
    }

    @Override
    default EntityTraversal<S, Edge> bothE(String... edgeLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.bothE(edgeLabels);
    }

    @Override
    default EntityTraversal<S, Vertex> toV(Direction direction) {
        return (EntityTraversal) EntityTraversalDsl.super.toV(direction);
    }

    @Override
    default EntityTraversal<S, Vertex> inV() {
        return (EntityTraversal) EntityTraversalDsl.super.inV();
    }

    @Override
    default EntityTraversal<S, Vertex> outV() {
        return (EntityTraversal) EntityTraversalDsl.super.outV();
    }

    @Override
    default EntityTraversal<S, Vertex> bothV() {
        return (EntityTraversal) EntityTraversalDsl.super.bothV();
    }

    @Override
    default EntityTraversal<S, Vertex> otherV() {
        return (EntityTraversal) EntityTraversalDsl.super.otherV();
    }

    @Override
    default EntityTraversal<S, E> order() {
        return (EntityTraversal) EntityTraversalDsl.super.order();
    }

    @Override
    default EntityTraversal<S, E> order(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.order(scope);
    }

    @Override
    default <E2> EntityTraversal<S, ? extends Property<E2>> properties(String... propertyKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.properties(propertyKeys);
    }

    @Override
    default <E2> EntityTraversal<S, E2> values(String... propertyKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.values(propertyKeys);
    }

    @Override
    default <E2> EntityTraversal<S, Map<String, E2>> propertyMap(String... propertyKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.propertyMap(propertyKeys);
    }

    @Override
    default <E2> EntityTraversal<S, Map<Object, E2>> elementMap(String... propertyKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.elementMap(propertyKeys);
    }

    @Override
    default <E2> EntityTraversal<S, Map<Object, E2>> valueMap(String... propertyKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.valueMap(propertyKeys);
    }

    @Override
    default <E2> EntityTraversal<S, Map<Object, E2>> valueMap(boolean includeTokens,
                                                              String... propertyKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.valueMap(includeTokens,propertyKeys);
    }

    @Override
    default EntityTraversal<S, String> key() {
        return (EntityTraversal) EntityTraversalDsl.super.key();
    }

    @Override
    default <E2> EntityTraversal<S, E2> value() {
        return (EntityTraversal) EntityTraversalDsl.super.value();
    }

    @Override
    default EntityTraversal<S, Path> path() {
        return (EntityTraversal) EntityTraversalDsl.super.path();
    }

    @Override
    default <E2> EntityTraversal<S, Map<String, E2>> match(Traversal<?, ?>... matchTraversals) {
        return (EntityTraversal) EntityTraversalDsl.super.match(matchTraversals);
    }

    @Override
    default <E2> EntityTraversal<S, E2> sack() {
        return (EntityTraversal) EntityTraversalDsl.super.sack();
    }

    @Override
    default EntityTraversal<S, Integer> loops() {
        return (EntityTraversal) EntityTraversalDsl.super.loops();
    }

    @Override
    default EntityTraversal<S, Integer> loops(String loopName) {
        return (EntityTraversal) EntityTraversalDsl.super.loops(loopName);
    }

    @Override
    default <E2> EntityTraversal<S, Map<String, E2>> project(String projectKey,
                                                             String... otherProjectKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.project(projectKey,otherProjectKeys);
    }

    @Override
    default <E2> EntityTraversal<S, Map<String, E2>> select(Pop pop, String selectKey1,
                                                            String selectKey2, String... otherSelectKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.select(pop,selectKey1,selectKey2,otherSelectKeys);
    }

    @Override
    default <E2> EntityTraversal<S, Map<String, E2>> select(String selectKey1, String selectKey2,
                                                            String... otherSelectKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.select(selectKey1,selectKey2,otherSelectKeys);
    }

    @Override
    default <E2> EntityTraversal<S, E2> select(Pop pop, String selectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.select(pop,selectKey);
    }

    @Override
    default <E2> EntityTraversal<S, E2> select(String selectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.select(selectKey);
    }

    @Override
    default <E2> EntityTraversal<S, E2> select(Pop pop, Traversal<S, E2> keyTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.select(pop,keyTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, E2> select(Traversal<S, E2> keyTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.select(keyTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, Collection<E2>> select(Column column) {
        return (EntityTraversal) EntityTraversalDsl.super.select(column);
    }

    @Override
    default <E2> EntityTraversal<S, E2> unfold() {
        return (EntityTraversal) EntityTraversalDsl.super.unfold();
    }

    @Override
    default EntityTraversal<S, List<E>> fold() {
        return (EntityTraversal) EntityTraversalDsl.super.fold();
    }

    @Override
    default <E2> EntityTraversal<S, E2> fold(E2 seed, BiFunction<E2, E, E2> foldFunction) {
        return (EntityTraversal) EntityTraversalDsl.super.fold(seed,foldFunction);
    }

    @Override
    default EntityTraversal<S, Long> count() {
        return (EntityTraversal) EntityTraversalDsl.super.count();
    }

    @Override
    default EntityTraversal<S, Long> count(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.count(scope);
    }

    @Override
    default <E2 extends Number> EntityTraversal<S, E2> sum() {
        return (EntityTraversal) EntityTraversalDsl.super.sum();
    }

    @Override
    default <E2 extends Number> EntityTraversal<S, E2> sum(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.sum(scope);
    }

    @Override
    default <E2 extends Comparable> EntityTraversal<S, E2> max() {
        return (EntityTraversal) EntityTraversalDsl.super.max();
    }

    @Override
    default <E2 extends Comparable> EntityTraversal<S, E2> max(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.max(scope);
    }

    @Override
    default <E2 extends Comparable> EntityTraversal<S, E2> min() {
        return (EntityTraversal) EntityTraversalDsl.super.min();
    }

    @Override
    default <E2 extends Comparable> EntityTraversal<S, E2> min(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.min(scope);
    }

    @Override
    default <E2 extends Number> EntityTraversal<S, E2> mean() {
        return (EntityTraversal) EntityTraversalDsl.super.mean();
    }

    @Override
    default <E2 extends Number> EntityTraversal<S, E2> mean(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.mean(scope);
    }

    @Override
    default <K, V> EntityTraversal<S, Map<K, V>> group() {
        return (EntityTraversal) EntityTraversalDsl.super.group();
    }

    @Override
    default <K> EntityTraversal<S, Map<K, Long>> groupCount() {
        return (EntityTraversal) EntityTraversalDsl.super.groupCount();
    }

    @Override
    default EntityTraversal<S, Tree> tree() {
        return (EntityTraversal) EntityTraversalDsl.super.tree();
    }

    @Override
    default EntityTraversal<S, Vertex> addV(String vertexLabel) {
        return (EntityTraversal) EntityTraversalDsl.super.addV(vertexLabel);
    }

    @Override
    default EntityTraversal<S, Vertex> addV(Traversal<?, String> vertexLabelTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.addV(vertexLabelTraversal);
    }

    @Override
    default EntityTraversal<S, Vertex> addV() {
        return (EntityTraversal) EntityTraversalDsl.super.addV();
    }

    @Override
    default EntityTraversal<S, Edge> addE(String edgeLabel) {
        return (EntityTraversal) EntityTraversalDsl.super.addE(edgeLabel);
    }

    @Override
    default EntityTraversal<S, Edge> addE(Traversal<?, String> edgeLabelTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.addE(edgeLabelTraversal);
    }

    @Override
    default EntityTraversal<S, E> to(String toStepLabel) {
        return (EntityTraversal) EntityTraversalDsl.super.to(toStepLabel);
    }

    @Override
    default EntityTraversal<S, E> from(String fromStepLabel) {
        return (EntityTraversal) EntityTraversalDsl.super.from(fromStepLabel);
    }

    @Override
    default EntityTraversal<S, E> to(Traversal<?, Vertex> toVertex) {
        return (EntityTraversal) EntityTraversalDsl.super.to(toVertex);
    }

    @Override
    default EntityTraversal<S, E> from(Traversal<?, Vertex> fromVertex) {
        return (EntityTraversal) EntityTraversalDsl.super.from(fromVertex);
    }

    @Override
    default EntityTraversal<S, E> to(Vertex toVertex) {
        return (EntityTraversal) EntityTraversalDsl.super.to(toVertex);
    }

    @Override
    default EntityTraversal<S, E> from(Vertex fromVertex) {
        return (EntityTraversal) EntityTraversalDsl.super.from(fromVertex);
    }

    @Override
    default EntityTraversal<S, Double> math(String expression) {
        return (EntityTraversal) EntityTraversalDsl.super.math(expression);
    }

    @Override
    default EntityTraversal<S, E> filter(Predicate<Traverser<E>> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.filter(predicate);
    }

    @Override
    default EntityTraversal<S, E> filter(Traversal<?, ?> filterTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.filter(filterTraversal);
    }

    @Override
    default EntityTraversal<S, E> or(Traversal<?, ?>... orTraversals) {
        return (EntityTraversal) EntityTraversalDsl.super.or(orTraversals);
    }

    @Override
    default EntityTraversal<S, E> and(Traversal<?, ?>... andTraversals) {
        return (EntityTraversal) EntityTraversalDsl.super.and(andTraversals);
    }

    @Override
    default EntityTraversal<S, E> inject(E... injections) {
        return (EntityTraversal) EntityTraversalDsl.super.inject(injections);
    }

    @Override
    default EntityTraversal<S, E> dedup(Scope scope, String... dedupLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.dedup(scope,dedupLabels);
    }

    @Override
    default EntityTraversal<S, E> dedup(String... dedupLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.dedup(dedupLabels);
    }

    @Override
    default EntityTraversal<S, E> where(String startKey, P<String> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.where(startKey,predicate);
    }

    @Override
    default EntityTraversal<S, E> where(P<String> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.where(predicate);
    }

    @Override
    default EntityTraversal<S, E> where(Traversal<?, ?> whereTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.where(whereTraversal);
    }

    @Override
    default EntityTraversal<S, E> has(String propertyKey, P<?> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.has(propertyKey,predicate);
    }

    @Override
    default EntityTraversal<S, E> has(T accessor, P<?> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.has(accessor,predicate);
    }

    @Override
    default EntityTraversal<S, E> has(String propertyKey, Object value) {
        return (EntityTraversal) EntityTraversalDsl.super.has(propertyKey,value);
    }

    @Override
    default EntityTraversal<S, E> has(T accessor, Object value) {
        return (EntityTraversal) EntityTraversalDsl.super.has(accessor,value);
    }

    @Override
    default EntityTraversal<S, E> has(String label, String propertyKey, P<?> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.has(label,propertyKey,predicate);
    }

    @Override
    default EntityTraversal<S, E> has(String label, String propertyKey, Object value) {
        return (EntityTraversal) EntityTraversalDsl.super.has(label,propertyKey,value);
    }

    @Override
    default EntityTraversal<S, E> has(T accessor, Traversal<?, ?> propertyTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.has(accessor,propertyTraversal);
    }

    @Override
    default EntityTraversal<S, E> has(String propertyKey, Traversal<?, ?> propertyTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.has(propertyKey,propertyTraversal);
    }

    @Override
    default EntityTraversal<S, E> has(String propertyKey) {
        return (EntityTraversal) EntityTraversalDsl.super.has(propertyKey);
    }

    @Override
    default EntityTraversal<S, E> hasNot(String propertyKey) {
        return (EntityTraversal) EntityTraversalDsl.super.hasNot(propertyKey);
    }

    @Override
    default EntityTraversal<S, E> hasLabel(String label, String... otherLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.hasLabel(label,otherLabels);
    }

    @Override
    default EntityTraversal<S, E> hasLabel(P<String> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.hasLabel(predicate);
    }

    @Override
    default EntityTraversal<S, E> hasId(Object id, Object... otherIds) {
        return (EntityTraversal) EntityTraversalDsl.super.hasId(id,otherIds);
    }

    @Override
    default EntityTraversal<S, E> hasId(P<Object> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.hasId(predicate);
    }

    @Override
    default EntityTraversal<S, E> hasKey(String label, String... otherLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.hasKey(label,otherLabels);
    }

    @Override
    default EntityTraversal<S, E> hasKey(P<String> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.hasKey(predicate);
    }

    @Override
    default EntityTraversal<S, E> hasValue(Object value, Object... otherValues) {
        return (EntityTraversal) EntityTraversalDsl.super.hasValue(value,otherValues);
    }

    @Override
    default EntityTraversal<S, E> hasValue(P<Object> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.hasValue(predicate);
    }

    @Override
    default EntityTraversal<S, E> is(P<E> predicate) {
        return (EntityTraversal) EntityTraversalDsl.super.is(predicate);
    }

    @Override
    default EntityTraversal<S, E> is(Object value) {
        return (EntityTraversal) EntityTraversalDsl.super.is(value);
    }

    @Override
    default EntityTraversal<S, E> not(Traversal<?, ?> notTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.not(notTraversal);
    }

    @Override
    default EntityTraversal<S, E> coin(double probability) {
        return (EntityTraversal) EntityTraversalDsl.super.coin(probability);
    }

    @Override
    default EntityTraversal<S, E> range(long low, long high) {
        return (EntityTraversal) EntityTraversalDsl.super.range(low,high);
    }

    @Override
    default <E2> EntityTraversal<S, E2> range(Scope scope, long low, long high) {
        return (EntityTraversal) EntityTraversalDsl.super.range(scope,low,high);
    }

    @Override
    default EntityTraversal<S, E> limit(long limit) {
        return (EntityTraversal) EntityTraversalDsl.super.limit(limit);
    }

    @Override
    default <E2> EntityTraversal<S, E2> limit(Scope scope, long limit) {
        return (EntityTraversal) EntityTraversalDsl.super.limit(scope,limit);
    }

    @Override
    default EntityTraversal<S, E> tail() {
        return (EntityTraversal) EntityTraversalDsl.super.tail();
    }

    @Override
    default EntityTraversal<S, E> tail(long limit) {
        return (EntityTraversal) EntityTraversalDsl.super.tail(limit);
    }

    @Override
    default <E2> EntityTraversal<S, E2> tail(Scope scope) {
        return (EntityTraversal) EntityTraversalDsl.super.tail(scope);
    }

    @Override
    default <E2> EntityTraversal<S, E2> tail(Scope scope, long limit) {
        return (EntityTraversal) EntityTraversalDsl.super.tail(scope,limit);
    }

    @Override
    default EntityTraversal<S, E> skip(long skip) {
        return (EntityTraversal) EntityTraversalDsl.super.skip(skip);
    }

    @Override
    default <E2> EntityTraversal<S, E2> skip(Scope scope, long skip) {
        return (EntityTraversal) EntityTraversalDsl.super.skip(scope,skip);
    }

    @Override
    default EntityTraversal<S, E> timeLimit(long timeLimit) {
        return (EntityTraversal) EntityTraversalDsl.super.timeLimit(timeLimit);
    }

    @Override
    default EntityTraversal<S, E> simplePath() {
        return (EntityTraversal) EntityTraversalDsl.super.simplePath();
    }

    @Override
    default EntityTraversal<S, E> cyclicPath() {
        return (EntityTraversal) EntityTraversalDsl.super.cyclicPath();
    }

    @Override
    default EntityTraversal<S, E> sample(int amountToSample) {
        return (EntityTraversal) EntityTraversalDsl.super.sample(amountToSample);
    }

    @Override
    default EntityTraversal<S, E> sample(Scope scope, int amountToSample) {
        return (EntityTraversal) EntityTraversalDsl.super.sample(scope,amountToSample);
    }

    @Override
    default EntityTraversal<S, E> drop() {
        return (EntityTraversal) EntityTraversalDsl.super.drop();
    }

    @Override
    default EntityTraversal<S, E> sideEffect(Consumer<Traverser<E>> consumer) {
        return (EntityTraversal) EntityTraversalDsl.super.sideEffect(consumer);
    }

    @Override
    default EntityTraversal<S, E> sideEffect(Traversal<?, ?> sideEffectTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.sideEffect(sideEffectTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, E2> cap(String sideEffectKey, String... sideEffectKeys) {
        return (EntityTraversal) EntityTraversalDsl.super.cap(sideEffectKey,sideEffectKeys);
    }

    @Override
    default EntityTraversal<S, Edge> subgraph(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.subgraph(sideEffectKey);
    }

    @Override
    default EntityTraversal<S, E> aggregate(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.aggregate(sideEffectKey);
    }

    @Override
    default EntityTraversal<S, E> aggregate(Scope scope, String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.aggregate(scope,sideEffectKey);
    }

    @Override
    default EntityTraversal<S, E> group(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.group(sideEffectKey);
    }

    @Override
    default EntityTraversal<S, E> groupCount(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.groupCount(sideEffectKey);
    }

    @Override
    default EntityTraversal<S, E> tree(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.tree(sideEffectKey);
    }

    @Override
    default <V, U> EntityTraversal<S, E> sack(BiFunction<V, U, V> sackOperator) {
        return (EntityTraversal) EntityTraversalDsl.super.sack(sackOperator);
    }

    @Override
    default EntityTraversal<S, E> store(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.store(sideEffectKey);
    }

    @Override
    default EntityTraversal<S, E> profile(String sideEffectKey) {
        return (EntityTraversal) EntityTraversalDsl.super.profile(sideEffectKey);
    }

    @Override
    default EntityTraversal<S, TraversalMetrics> profile() {
        return (EntityTraversal) EntityTraversalDsl.super.profile();
    }

    @Override
    default EntityTraversal<S, E> none() {
        return (EntityTraversal) EntityTraversalDsl.super.none();
    }

    @Override
    default EntityTraversal<S, E> property(VertexProperty.Cardinality cardinality, Object key,
                                           Object value, Object... keyValues) {
        return (EntityTraversal) EntityTraversalDsl.super.property(cardinality,key,value,keyValues);
    }

    @Override
    default EntityTraversal<S, E> property(Object key, Object value, Object... keyValues) {
        return (EntityTraversal) EntityTraversalDsl.super.property(key,value,keyValues);
    }

    @Override
    default <M, E2> EntityTraversal<S, E2> branch(Traversal<?, M> branchTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.branch(branchTraversal);
    }

    @Override
    default <M, E2> EntityTraversal<S, E2> branch(Function<Traverser<E>, M> function) {
        return (EntityTraversal) EntityTraversalDsl.super.branch(function);
    }

    @Override
    default <M, E2> EntityTraversal<S, E2> choose(Traversal<?, M> choiceTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.choose(choiceTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, E2> choose(Traversal<?, ?> traversalPredicate,
                                               Traversal<?, E2> trueChoice, Traversal<?, E2> falseChoice) {
        return (EntityTraversal) EntityTraversalDsl.super.choose(traversalPredicate,trueChoice,falseChoice);
    }

    @Override
    default <E2> EntityTraversal<S, E2> choose(Traversal<?, ?> traversalPredicate,
                                               Traversal<?, E2> trueChoice) {
        return (EntityTraversal) EntityTraversalDsl.super.choose(traversalPredicate,trueChoice);
    }

    @Override
    default <M, E2> EntityTraversal<S, E2> choose(Function<E, M> choiceFunction) {
        return (EntityTraversal) EntityTraversalDsl.super.choose(choiceFunction);
    }

    @Override
    default <E2> EntityTraversal<S, E2> choose(Predicate<E> choosePredicate,
                                               Traversal<?, E2> trueChoice, Traversal<?, E2> falseChoice) {
        return (EntityTraversal) EntityTraversalDsl.super.choose(choosePredicate,trueChoice,falseChoice);
    }

    @Override
    default <E2> EntityTraversal<S, E2> choose(Predicate<E> choosePredicate,
                                               Traversal<?, E2> trueChoice) {
        return (EntityTraversal) EntityTraversalDsl.super.choose(choosePredicate,trueChoice);
    }

    @Override
    default <E2> EntityTraversal<S, E2> optional(Traversal<?, E2> optionalTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.optional(optionalTraversal);
    }

    @Override
    default <E2> EntityTraversal<S, E2> union(Traversal<?, E2>... unionTraversals) {
        return (EntityTraversal) EntityTraversalDsl.super.union(unionTraversals);
    }

    @Override
    default <E2> EntityTraversal<S, E2> coalesce(Traversal<?, E2>... coalesceTraversals) {
        return (EntityTraversal) EntityTraversalDsl.super.coalesce(coalesceTraversals);
    }

    @Override
    default EntityTraversal<S, E> repeat(Traversal<?, E> repeatTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.repeat(repeatTraversal);
    }

    @Override
    default EntityTraversal<S, E> repeat(String loopName, Traversal<?, E> repeatTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.repeat(loopName,repeatTraversal);
    }

    @Override
    default EntityTraversal<S, E> emit(Traversal<?, ?> emitTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.emit(emitTraversal);
    }

    @Override
    default EntityTraversal<S, E> emit(Predicate<Traverser<E>> emitPredicate) {
        return (EntityTraversal) EntityTraversalDsl.super.emit(emitPredicate);
    }

    @Override
    default EntityTraversal<S, E> emit() {
        return (EntityTraversal) EntityTraversalDsl.super.emit();
    }

    @Override
    default EntityTraversal<S, E> until(Traversal<?, ?> untilTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.until(untilTraversal);
    }

    @Override
    default EntityTraversal<S, E> until(Predicate<Traverser<E>> untilPredicate) {
        return (EntityTraversal) EntityTraversalDsl.super.until(untilPredicate);
    }

    @Override
    default EntityTraversal<S, E> times(int maxLoops) {
        return (EntityTraversal) EntityTraversalDsl.super.times(maxLoops);
    }

    @Override
    default <E2> EntityTraversal<S, E2> local(Traversal<?, E2> localTraversal) {
        return (EntityTraversal) EntityTraversalDsl.super.local(localTraversal);
    }

    @Override
    default EntityTraversal<S, E> pageRank() {
        return (EntityTraversal) EntityTraversalDsl.super.pageRank();
    }

    @Override
    default EntityTraversal<S, E> pageRank(double alpha) {
        return (EntityTraversal) EntityTraversalDsl.super.pageRank(alpha);
    }

    @Override
    default EntityTraversal<S, E> peerPressure() {
        return (EntityTraversal) EntityTraversalDsl.super.peerPressure();
    }

    @Override
    default EntityTraversal<S, E> connectedComponent() {
        return (EntityTraversal) EntityTraversalDsl.super.connectedComponent();
    }

    @Override
    default EntityTraversal<S, Path> shortestPath() {
        return (EntityTraversal) EntityTraversalDsl.super.shortestPath();
    }

    @Override
    default EntityTraversal<S, E> program(VertexProgram<?> vertexProgram) {
        return (EntityTraversal) EntityTraversalDsl.super.program(vertexProgram);
    }

    @Override
    default EntityTraversal<S, E> as(String stepLabel, String... stepLabels) {
        return (EntityTraversal) EntityTraversalDsl.super.as(stepLabel,stepLabels);
    }

    @Override
    default EntityTraversal<S, E> barrier() {
        return (EntityTraversal) EntityTraversalDsl.super.barrier();
    }

    @Override
    default EntityTraversal<S, E> barrier(int maxBarrierSize) {
        return (EntityTraversal) EntityTraversalDsl.super.barrier(maxBarrierSize);
    }

    @Override
    default <E2> EntityTraversal<S, E2> index() {
        return (EntityTraversal) EntityTraversalDsl.super.index();
    }

    @Override
    default EntityTraversal<S, E> barrier(Consumer<TraverserSet<Object>> barrierConsumer) {
        return (EntityTraversal) EntityTraversalDsl.super.barrier(barrierConsumer);
    }

    @Override
    default EntityTraversal<S, E> with(String key) {
        return (EntityTraversal) EntityTraversalDsl.super.with(key);
    }

    @Override
    default EntityTraversal<S, E> with(String key, Object value) {
        return (EntityTraversal) EntityTraversalDsl.super.with(key,value);
    }

    @Override
    default EntityTraversal<S, E> by() {
        return (EntityTraversal) EntityTraversalDsl.super.by();
    }

    @Override
    default EntityTraversal<S, E> by(Traversal<?, ?> traversal) {
        return (EntityTraversal) EntityTraversalDsl.super.by(traversal);
    }

    @Override
    default EntityTraversal<S, E> by(T token) {
        return (EntityTraversal) EntityTraversalDsl.super.by(token);
    }

    @Override
    default EntityTraversal<S, E> by(String key) {
        return (EntityTraversal) EntityTraversalDsl.super.by(key);
    }

    @Override
    default <V> EntityTraversal<S, E> by(Function<V, Object> function) {
        return (EntityTraversal) EntityTraversalDsl.super.by(function);
    }

    @Override
    default <V> EntityTraversal<S, E> by(Traversal<?, ?> traversal, Comparator<V> comparator) {
        return (EntityTraversal) EntityTraversalDsl.super.by(traversal,comparator);
    }

    @Override
    default EntityTraversal<S, E> by(Comparator<E> comparator) {
        return (EntityTraversal) EntityTraversalDsl.super.by(comparator);
    }

    @Override
    default EntityTraversal<S, E> by(Order order) {
        return (EntityTraversal) EntityTraversalDsl.super.by(order);
    }

    @Override
    default <V> EntityTraversal<S, E> by(String key, Comparator<V> comparator) {
        return (EntityTraversal) EntityTraversalDsl.super.by(key,comparator);
    }

    @Override
    default <U> EntityTraversal<S, E> by(Function<U, Object> function, Comparator comparator) {
        return (EntityTraversal) EntityTraversalDsl.super.by(function,comparator);
    }

    @Override
    default <M, E2> EntityTraversal<S, E> option(M pick, Traversal<?, E2> traversalOption) {
        return (EntityTraversal) EntityTraversalDsl.super.option(pick,traversalOption);
    }

    @Override
    default <E2> EntityTraversal<S, E> option(Traversal<?, E2> traversalOption) {
        return (EntityTraversal) EntityTraversalDsl.super.option(traversalOption);
    }

    @Override
    default EntityTraversal<S, E> read() {
        return (EntityTraversal) EntityTraversalDsl.super.read();
    }

    @Override
    default EntityTraversal<S, E> write() {
        return (EntityTraversal) EntityTraversalDsl.super.write();
    }

    @Override
    default EntityTraversal<S, E> iterate() {
        EntityTraversalDsl.super.iterate();
        return this;
    }
}

