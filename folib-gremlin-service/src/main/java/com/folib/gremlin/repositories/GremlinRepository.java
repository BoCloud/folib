package com.folib.gremlin.repositories;

import java.util.Optional;
import java.util.function.Supplier;



import com.folib.data.domain.DomainObject;
import com.folib.gremlin.dsl.EntityTraversal;
import com.folib.gremlin.dsl.EntityTraversalSource;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.apache.tinkerpop.gremlin.structure.Element;
import org.apache.tinkerpop.gremlin.structure.Graph;
import com.folib.gremlin.adapters.EntityTraversalAdapter;
import com.folib.gremlin.tx.TransactionContext;
import org.neo4j.ogm.session.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.repository.CrudRepository;

/**
 * Base implementation for Gremlin repositories.
 *
 * @author veadan
 */
@Transactional
public abstract class GremlinRepository<S extends Element, E extends DomainObject> implements CrudRepository<E, String>
{

    private static final Logger logger = LoggerFactory.getLogger(GremlinRepository.class);

    @Inject
    @TransactionContext
    private Graph graph;

    @Inject
    protected Session session;
    
    
    protected EntityTraversalSource g()
    {
        return graph.traversal(EntityTraversalSource.class);
    }

    protected abstract EntityTraversal<S, S> start(E entity, Supplier<EntityTraversalSource> g);

    protected abstract EntityTraversal<S, S> start(Supplier<EntityTraversalSource> g);

    public <R extends E> R save(Supplier<EntityTraversalSource> g, R entity)
    {
        String uuid = merge(g, entity);

        return (R) findById(g, uuid).get();
    }

    public abstract String merge(Supplier<EntityTraversalSource> g, E entity);

    public Optional<E> findById(Supplier<EntityTraversalSource> g, String uuid)
    {
        String label = adapter().label();
        EntityTraversal<S, E> traversal = start(g).findById(uuid, label)
                                                  .map(adapter().fold());
        if (!traversal.hasNext())
        {
            return Optional.empty();
        }

        return Optional.of(traversal.next());
    }

    public Optional<E> findById(String uuid)
    {
        return findById(this::g, uuid);
    }

    @Override
    public <S extends E> Iterable<S> saveAll(Iterable<S> entities)
    {
        throw new UnsupportedOperationException("TODO implement");
    }

    @Override
    public boolean existsById(String id)
    {
        throw new UnsupportedOperationException("TODO implement");
    }

    @Override
    public Iterable<E> findAll()
    {
        throw new UnsupportedOperationException("TODO implement");
    }

    @Override
    public Iterable<E> findAllById(Iterable<String> ids)
    {
        throw new UnsupportedOperationException("TODO implement");
    }

    @Override
    public long count()
    {
        throw new UnsupportedOperationException("TODO implement");
    }

    @Override
    public void deleteById(String id)
    {
        String label = adapter().label();
        start(this::g).findById(id, label)
                .flatMap(adapter().cascade())
                .dedup()
                .info("Delete")
                .drop()
                .iterate();
        session.clear();
    }

    @Override
    public void delete(E entity)
    {
        deleteById(entity.getUuid());
    }

    @Override
    public void deleteAll(Iterable<? extends E> entities)
    {
        for (E entity : entities)
        {
            delete(entity);
        }
    }

    @Override
    public void deleteAll()
    {
        throw new UnsupportedOperationException("TODO implement");
    }

    protected abstract EntityTraversalAdapter<S, E> adapter();

}
