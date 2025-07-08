package com.folib.gremlin.adapters;

import com.folib.gremlin.adapters.EntityTraversalAdapter;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.folib.data.domain.DomainObject;

/**
 * @author veadan
 */
public interface EntityUpwardHierarchyNodeAdapter<E extends DomainObject> extends EntityTraversalAdapter<Vertex, E>
{

    Class<? extends E> entityClass();

}
