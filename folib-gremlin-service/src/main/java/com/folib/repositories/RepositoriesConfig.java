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

import java.util.Optional;

import com.folib.config.janusgraph.DelegatingIdBlockQueueSupplier;
import org.apache.tinkerpop.gremlin.structure.Graph;
import com.folib.gremlin.tx.GraphTransaction;
import com.folib.gremlin.tx.TransactionContext;
import org.janusgraph.core.JanusGraph;
import org.neo4j.ogm.session.Session;
import org.neo4j.ogm.session.SessionFactory;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.neo4j.repository.config.EnableNeo4jRepositories;
import org.springframework.data.neo4j.transaction.Neo4jTransactionManager;


@Configuration
@ComponentScan
@EnableNeo4jRepositories
public class RepositoriesConfig
{

    @Bean
    public TransactionalIdBlockQueueSuppiler idBlockQueueSessionFactory(JanusGraph graph,
                                                                        DelegatingIdBlockQueueSupplier idBlockQueueSupplier)
        throws Exception
    {
        TransactionalIdBlockQueueSuppiler transactionalIdBlockQueueSuppiler = new TransactionalIdBlockQueueSuppiler(graph);
        idBlockQueueSupplier.setTarget(() -> Optional.of(transactionalIdBlockQueueSuppiler.get())
                                                     .map(IdBlockQueueSession::getIdBlockQueueName)
                                                     .orElse(null));

        return transactionalIdBlockQueueSuppiler;
    }

    @Bean
    public SessionFactory sessionFactory(TransactionalIdBlockQueueSuppiler idBlockQueueSessionFactory)
        throws Exception
    {
        return idBlockQueueSessionFactory.getSessionFactory("default");
    }

    @Bean
    public Session session(TransactionalIdBlockQueueSuppiler idBlockQueueSessionFactory) throws Exception {
        return sessionFactory(idBlockQueueSessionFactory).openSession();
    }

    @Bean
    @Primary
    public Neo4jTransactionManager defaultTransactionManager(TransactionalIdBlockQueueSuppiler idBlockQueueSessionFactory)
        throws Exception
    {
        return new Neo4jTransactionManager(idBlockQueueSessionFactory.getSessionFactory("default"));
    }

    @Bean
    @Qualifier("cronJobTransactionManager")
    public Neo4jTransactionManager cronJobTransactionManager(TransactionalIdBlockQueueSuppiler idBlockQueueSessionFactory)
        throws Exception
    {
        return new Neo4jTransactionManager(idBlockQueueSessionFactory.getSessionFactory("cron-job"));
    }

    @Bean
    @TransactionContext
    public Graph graphTransaction(TransactionalIdBlockQueueSuppiler sessionFactory)
    {
        return new GraphTransaction(sessionFactory);
    }

}
