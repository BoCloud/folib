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
import org.apache.tinkerpop.gremlin.process.computer.Computer;
import org.apache.tinkerpop.gremlin.process.computer.GraphComputer;
import org.apache.tinkerpop.gremlin.process.remote.RemoteConnection;
import org.apache.tinkerpop.gremlin.process.traversal.Traversal;
import org.apache.tinkerpop.gremlin.process.traversal.TraversalStrategies;
import org.apache.tinkerpop.gremlin.process.traversal.TraversalStrategy;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.step.map.AddEdgeStartStep;
import org.apache.tinkerpop.gremlin.process.traversal.step.map.AddVertexStartStep;
import org.apache.tinkerpop.gremlin.process.traversal.step.map.GraphStep;
import org.apache.tinkerpop.gremlin.process.traversal.step.sideEffect.InjectStep;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Vertex;

import java.util.Optional;
import java.util.function.BinaryOperator;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

public class EntityTraversalSource extends EntityTraversalSourceDsl {
  public EntityTraversalSource(Graph graph) {
    super(graph);
  }

  public EntityTraversalSource(Graph graph, TraversalStrategies strategies) {
    super(graph, strategies);
  }

  public EntityTraversalSource(RemoteConnection connection) {
    super(connection);
  }

  @Override
  public EntityTraversalSource clone() {
    return (EntityTraversalSource) super.clone();
  }

  @Override
  public EntityTraversalSource with(String key) {
    return (EntityTraversalSource) super.with(key);
  }

  @Override
  public EntityTraversalSource with(String key, Object value) {
    return (EntityTraversalSource) super.with(key,value);
  }

  @Override
  public EntityTraversalSource withStrategies(TraversalStrategy... traversalStrategies) {
    return (EntityTraversalSource) super.withStrategies(traversalStrategies);
  }

  @Override
  public EntityTraversalSource withoutStrategies(Class<? extends TraversalStrategy>... traversalStrategyClasses) {
    return (EntityTraversalSource) super.withoutStrategies(traversalStrategyClasses);
  }

  @Override
  public EntityTraversalSource withComputer(Computer computer) {
    return (EntityTraversalSource) super.withComputer(computer);
  }

  @Override
  public EntityTraversalSource withComputer(Class<? extends GraphComputer> graphComputerClass) {
    return (EntityTraversalSource) super.withComputer(graphComputerClass);
  }

  @Override
  public EntityTraversalSource withComputer() {
    return (EntityTraversalSource) super.withComputer();
  }

  @Override
  public <A> EntityTraversalSource withSideEffect(String key, Supplier<A> initialValue,
      BinaryOperator<A> reducer) {
    return (EntityTraversalSource) super.withSideEffect(key,initialValue,reducer);
  }

  @Override
  public <A> EntityTraversalSource withSideEffect(String key, A initialValue,
      BinaryOperator<A> reducer) {
    return (EntityTraversalSource) super.withSideEffect(key,initialValue,reducer);
  }

  @Override
  public <A> EntityTraversalSource withSideEffect(String key, A initialValue) {
    return (EntityTraversalSource) super.withSideEffect(key,initialValue);
  }

  @Override
  public <A> EntityTraversalSource withSideEffect(String key, Supplier<A> initialValue) {
    return (EntityTraversalSource) super.withSideEffect(key,initialValue);
  }

  @Override
  public <A> EntityTraversalSource withSack(Supplier<A> initialValue,
      UnaryOperator<A> splitOperator, BinaryOperator<A> mergeOperator) {
    return (EntityTraversalSource) super.withSack(initialValue,splitOperator,mergeOperator);
  }

  @Override
  public <A> EntityTraversalSource withSack(A initialValue, UnaryOperator<A> splitOperator,
      BinaryOperator<A> mergeOperator) {
    return (EntityTraversalSource) super.withSack(initialValue,splitOperator,mergeOperator);
  }

  @Override
  public <A> EntityTraversalSource withSack(A initialValue) {
    return (EntityTraversalSource) super.withSack(initialValue);
  }

  @Override
  public <A> EntityTraversalSource withSack(Supplier<A> initialValue) {
    return (EntityTraversalSource) super.withSack(initialValue);
  }

  @Override
  public <A> EntityTraversalSource withSack(Supplier<A> initialValue,
      UnaryOperator<A> splitOperator) {
    return (EntityTraversalSource) super.withSack(initialValue,splitOperator);
  }

  @Override
  public <A> EntityTraversalSource withSack(A initialValue, UnaryOperator<A> splitOperator) {
    return (EntityTraversalSource) super.withSack(initialValue,splitOperator);
  }

  @Override
  public <A> EntityTraversalSource withSack(Supplier<A> initialValue,
      BinaryOperator<A> mergeOperator) {
    return (EntityTraversalSource) super.withSack(initialValue,mergeOperator);
  }

  @Override
  public <A> EntityTraversalSource withSack(A initialValue, BinaryOperator<A> mergeOperator) {
    return (EntityTraversalSource) super.withSack(initialValue,mergeOperator);
  }

  @Override
  public EntityTraversalSource withBulk(boolean useBulk) {
    return (EntityTraversalSource) super.withBulk(useBulk);
  }

  @Override
  public EntityTraversalSource withPath() {
    return (EntityTraversalSource) super.withPath();
  }

  @Override
  public EntityTraversal<Vertex, Vertex> V(DomainObject entity) {
    EntityTraversalSource clone = this.clone();
    return new DefaultEntityTraversal (clone, super.V(entity).asAdmin());
  }

  @Override
  public EntityTraversal<Vertex, Vertex> addV() {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.addV);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new AddVertexStartStep(traversal, (String) null));
  }

  @Override
  public EntityTraversal<Vertex, Vertex> addV(String label) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.addV, label);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new AddVertexStartStep(traversal, label));
  }

  @Override
  public EntityTraversal<Vertex, Vertex> addV(Traversal vertexLabelTraversal) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.addV, vertexLabelTraversal);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new AddVertexStartStep(traversal, vertexLabelTraversal));
  }

  @Override
  public EntityTraversal<Edge, Edge> addE(String label) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.addE, label);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new AddEdgeStartStep(traversal, label));
  }

  @Override
  public EntityTraversal<Edge, Edge> addE(Traversal edgeLabelTraversal) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.addE, edgeLabelTraversal);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new AddEdgeStartStep(traversal, edgeLabelTraversal));
  }

  @Override
  public EntityTraversal<Vertex, Vertex> V(Object... vertexIds) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.V, vertexIds);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new GraphStep(traversal, Vertex.class, true, vertexIds));
  }

  @Override
  public EntityTraversal<Edge, Edge> E(Object... edgeIds) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.E, edgeIds);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new GraphStep(traversal, Edge.class, true, edgeIds));
  }

  @Override
  public <S> EntityTraversal<S, S> inject(S... starts) {
    EntityTraversalSource clone = this.clone();
    clone.getBytecode().addStep(GraphTraversal.Symbols.inject, starts);
    DefaultEntityTraversal traversal = new DefaultEntityTraversal(clone);
    return (EntityTraversal) traversal.asAdmin().addStep(new InjectStep(traversal, starts));
  }

  @Override
  public Optional<Class<?>> getAnonymousTraversalClass() {
    return Optional.of(__.class);
  }
}
