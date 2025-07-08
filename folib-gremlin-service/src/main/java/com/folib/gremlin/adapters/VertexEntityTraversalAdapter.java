package com.folib.gremlin.adapters;

import com.folib.data.domain.DomainObject;
import org.apache.tinkerpop.gremlin.structure.Vertex;

/**
 * {@link EntityTraversalAdapter} for entities associated with vertices.
 *
 * @param <E>
 *
 * @author veadan
 */
public interface VertexEntityTraversalAdapter<E extends DomainObject> extends EntityTraversalAdapter<Vertex, E>
{

}
