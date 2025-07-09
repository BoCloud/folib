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

import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversal;
import org.apache.tinkerpop.gremlin.process.traversal.util.DefaultTraversal;
import org.apache.tinkerpop.gremlin.structure.Graph;

public class DefaultEntityTraversal<S, E> extends DefaultTraversal<S, E> implements EntityTraversal<S, E> {
  public DefaultEntityTraversal() {
    super();
  }

  public DefaultEntityTraversal(Graph graph) {
    super(graph);
  }

  public DefaultEntityTraversal(EntityTraversalSource traversalSource) {
    super(traversalSource);
  }

  public DefaultEntityTraversal(EntityTraversalSource traversalSource,
                                GraphTraversal.Admin traversal) {
    super(traversalSource, traversal.asAdmin());
  }

  @Override
  public EntityTraversal<S, E> iterate() {
    return (EntityTraversal) super.iterate();
  }

  @Override
  public GraphTraversal.Admin<S, E> asAdmin() {
    return (GraphTraversal.Admin) super.asAdmin();
  }

  @Override
  public DefaultEntityTraversal<S, E> clone() {
    return (DefaultEntityTraversal) super.clone();
  }
}
