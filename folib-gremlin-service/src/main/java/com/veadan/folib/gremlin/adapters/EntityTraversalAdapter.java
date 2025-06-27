package com.veadan.folib.gremlin.adapters;

import com.veadan.folib.data.domain.DomainObject;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.springframework.data.repository.Repository;

/**
 * Makes possible to translate {@link Repository} operations on underlying graph using Gremlin query language.
 * Every entity should have corresponding {@link EntityTraversalAdapter} implementation.
 *
 * @param <S>
 * @param <E>
 *
 * @author veadan
 */
public interface EntityTraversalAdapter<S extends Element, E extends DomainObject>
{

    /**
     * Element label associated with the entity type.
     *
     * @return target element label
     */
    String label();

    /**
     * Folds the current traversal into entity instance.
     *
     * @return traversal with entity instance
     */
    EntityTraversal<S, E> fold();

    /**
     * Unfolds the entity instance on graph.
     *
     * @param entity entity to unfold
     * @return traversal with element associated with the entity
     */
    UnfoldEntityTraversal<S, S> unfold(E entity);

    /**
     * Performs entity cascade operations on graph.
     *
     * @return traversal with cascaded graph elements
     */
    EntityTraversal<S, Element> cascade();

}
