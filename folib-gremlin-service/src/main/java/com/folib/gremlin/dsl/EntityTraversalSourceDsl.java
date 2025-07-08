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
