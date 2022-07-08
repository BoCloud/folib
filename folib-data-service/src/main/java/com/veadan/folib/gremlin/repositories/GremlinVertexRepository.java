package com.veadan.folib.gremlin.repositories;

import java.util.function.Supplier;

import javax.transaction.Transactional;

import com.veadan.folib.data.domain.DomainObject;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.gremlin.adapters.UnfoldEntityTraversal;

/**
 * Gremlin repository for Vertex based entities.
 *
 * @param <E>
 *
 * @author xuxinping
 */
@Transactional
public abstract class GremlinVertexRepository<E extends DomainObject> extends GremlinRepository<Vertex, E>
{

    public String merge(E entity)
    {
        return merge(this::g, entity);
    }

    @Override
    public String merge(Supplier<EntityTraversalSource> g,
                        E entity)
    {
        UnfoldEntityTraversal<Vertex, Vertex> unfoldTraversal = adapter().unfold(entity);
        Vertex resultVertex = start(entity, g).saveV(entity.getUuid(),
                                                     unfoldTraversal)
                                              .next();
        session.clear();

        return resultVertex.<String>property("uuid").value();
    }

    @Override
    public <R extends E> R save(R entity)
    {
        return save(this::g, entity);
    }

    /**
     * Start gremlin traversal from specified entity vertex.
     */
    @Override
    public EntityTraversal<Vertex, Vertex> start(E entity,
                                                 Supplier<EntityTraversalSource> g)
    {
        Long vertexId = entity.getNativeId();
        if (vertexId != null)
        {
            return g.get().V(vertexId);
        }
        return g.get().V();
    }

    /**
     * Start gremlin traversal using {@link EntityTraversalSource}.
     */
    @Override
    protected EntityTraversal<Vertex, Vertex> start(Supplier<EntityTraversalSource> g)
    {
        return g.get().V();
    }

}
