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
