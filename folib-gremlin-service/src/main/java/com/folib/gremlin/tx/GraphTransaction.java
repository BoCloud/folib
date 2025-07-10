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
package com.folib.gremlin.tx;

import java.util.Iterator;
import java.util.Optional;

import com.folib.repositories.TransactionalIdBlockQueueSuppiler;
import org.apache.commons.configuration2.Configuration;
import org.apache.tinkerpop.gremlin.process.computer.GraphComputer;
import org.apache.tinkerpop.gremlin.process.traversal.TraversalSource;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.apache.tinkerpop.gremlin.structure.Transaction;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.apache.tinkerpop.gremlin.structure.io.Io;
import org.apache.tinkerpop.gremlin.structure.io.Io.Builder;
import org.neo4j.ogm.session.Session;
import org.opencypher.gremlin.neo4j.ogm.transaction.GremlinTransaction;

/**
 * Exposes current thread bound {@link Graph} transaction.
 *
 * @author veadan
 */
public class GraphTransaction implements Graph
{


    private static final Class<GremlinTransaction> gremlinTransactionClass = GremlinTransaction.class;

    private final TransactionalIdBlockQueueSuppiler sessionFactory;


    public GraphTransaction(TransactionalIdBlockQueueSuppiler sessionFactory)
    {
        this.sessionFactory = sessionFactory;
    }

    @Override
    public Vertex addVertex(Object... keyValues)
    {
        return getCurrent().addVertex(keyValues);
    }

    @Override
    public Vertex addVertex(String label)
    {
        return getCurrent().addVertex(label);
    }

    @Override
    public <C extends GraphComputer> C compute(Class<C> graphComputerClass)
        throws IllegalArgumentException
    {
        return getCurrent().compute(graphComputerClass);
    }

    @Override
    public GraphComputer compute()
        throws IllegalArgumentException
    {
        return getCurrent().compute();
    }

    @Override
    public <C extends TraversalSource> C traversal(Class<C> traversalSourceClass)
    {
        return getCurrent().traversal(traversalSourceClass);
    }

    @Override
    public GraphTraversalSource traversal()
    {
        return getCurrent().traversal();
    }

    @Override
    public Iterator<Vertex> vertices(Object... vertexIds)
    {
        return getCurrent().vertices(vertexIds);
    }

    @Override
    public Iterator<Edge> edges(Object... edgeIds)
    {
        return getCurrent().edges(edgeIds);
    }

    @Override
    public Transaction tx()
    {
        return getCurrent().tx();
    }

    @Override
    public void close()
        throws Exception
    {
        getCurrent().close();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public <I extends Io> I io(Builder<I> builder)
    {
        return getCurrent().io(builder);
    }

    @Override
    public Variables variables()
    {
        return getCurrent().variables();
    }

    @Override
    public Configuration configuration()
    {
        return getCurrent().configuration();
    }

    @Override
    public Features features()
    {
        return getCurrent().features();
    }

    private Graph getCurrent()
    {
        return Optional.ofNullable(sessionFactory.get())
                       .map(Session::getTransaction)
                       .map(gremlinTransactionClass::cast)
                       .map(GremlinTransaction::getNativeTransaction)
                       .orElseThrow(() -> new IllegalStateException());
    }

}
