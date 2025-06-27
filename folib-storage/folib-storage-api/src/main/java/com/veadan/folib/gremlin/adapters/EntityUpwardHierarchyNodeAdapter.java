package com.veadan.folib.gremlin.adapters;

import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.data.domain.DomainObject;

/**
 * @author veadan
 */
public interface EntityUpwardHierarchyNodeAdapter<E extends DomainObject> extends EntityTraversalAdapter<Vertex, E>
{

    Class<? extends E> entityClass();

}
