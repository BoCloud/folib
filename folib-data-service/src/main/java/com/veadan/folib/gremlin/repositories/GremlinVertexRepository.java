package com.veadan.folib.gremlin.repositories;

import java.util.function.Supplier;



import com.veadan.folib.data.domain.DomainObject;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.dsl.EntityTraversalSource;
import jakarta.transaction.Transactional;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import com.veadan.folib.gremlin.adapters.UnfoldEntityTraversal;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    private static final Logger logger = LoggerFactory.getLogger(GremlinVertexRepository.class);


    public String merge(E entity)
    {
        return merge(this::g, entity);
    }

    @Override
    public String merge(Supplier<EntityTraversalSource> g,
                        E entity)
    {
        UnfoldEntityTraversal<Vertex, Vertex> unfoldTraversal = adapter().unfold(entity);
        start(entity, g).saveV(entity.getUuid(), unfoldTraversal).iterate();
        session.clear();
        return entity.getUuid();
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
