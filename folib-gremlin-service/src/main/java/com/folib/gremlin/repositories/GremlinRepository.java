/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
