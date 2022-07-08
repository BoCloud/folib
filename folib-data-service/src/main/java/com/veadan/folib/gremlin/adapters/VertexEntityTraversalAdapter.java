package com.veadan.folib.gremlin.adapters;

import com.veadan.folib.data.domain.DomainObject;
import org.apache.tinkerpop.gremlin.structure.Vertex;

/**
 * {@link EntityTraversalAdapter} for entities associated with vertices.
 *
 * @param <E>
 *
 * @author xuxinping
 */
public interface VertexEntityTraversalAdapter<E extends DomainObject> extends EntityTraversalAdapter<Vertex, E>
{

}
