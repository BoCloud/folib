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
package com.folib.repositories;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import org.janusgraph.core.JanusGraph;
import org.neo4j.ogm.driver.Driver;
import org.neo4j.ogm.session.Session;
import org.neo4j.ogm.session.SessionFactory;
import org.opencypher.gremlin.neo4j.ogm.GremlinGraphDriver;
import org.springframework.core.InfrastructureProxy;
import org.springframework.data.neo4j.transaction.SessionHolder;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.folib.db.server.janusgraph.TransactionalVertexIDAssigner;

/**
 * Concrete implementation of `idBlockQueue` supplier for
 * {@link TransactionalVertexIDAssigner}. The `idBlockQueue`s are tied to the
 * particular {@link PlatformTransactionManager} instance which is used within
 * application.
 *
 * @see TransactionalVertexIDAssigner
 * @see IdBlockQueueSession
 * @see RepositoriesConfig#cronJobTransactionManager(TransactionalIdBlockQueueSuppiler)
 * @see RepositoriesConfig#defaultTransactionManager(TransactionalIdBlockQueueSuppiler)
 *
 * @author veadan
 */
public class TransactionalIdBlockQueueSuppiler implements Supplier<IdBlockQueueSession>
{

    private static final String[] PACKAGES = new String[] { "com.folib.domain",
                                                            "com.folib.artifact.coordinates" };

    private final Driver driver;
    private final Map<String, SessionFactory> sessionFactoryMap = new ConcurrentHashMap<>();

    public TransactionalIdBlockQueueSuppiler(JanusGraph graph)
    {
        this.driver = new GremlinGraphDriver(graph.tx());
    }

    public SessionFactory getSessionFactory(String idBlockName)
    {
        return sessionFactoryMap.computeIfAbsent(idBlockName, (name) -> new IdBlockQueueSessionFactory(name, driver, PACKAGES));
    }

    @Override
    public IdBlockQueueSession get()
    {
        SessionHolder sessionHolder = (SessionHolder) TransactionSynchronizationManager.getResource(this);
        return Optional.ofNullable(sessionHolder)
                       .map(SessionHolder::getSession)
                       .map(IdBlockQueueSession.class::cast)
                       .orElse(null);
    }

    private class IdBlockQueueSessionFactory extends SessionFactory implements InfrastructureProxy
    {

        private final String idBlockName;

        public IdBlockQueueSessionFactory(String idBlockName,
                                          Driver driver,
                                          String... packages)
        {
            super(driver, packages);
            this.idBlockName = idBlockName;
        }

        @Override
        public Session openSession()
        {
            return new IdBlockQueueSession(idBlockName, super.openSession());
        }

        @Override
        public Object getWrappedObject()
        {
            return TransactionalIdBlockQueueSuppiler.this;
        }

    }
}
