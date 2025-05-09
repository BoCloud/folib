package com.veadan.folib.repositories;

import java.util.Optional;

import com.veadan.folib.config.janusgraph.DelegatingIdBlockQueueSupplier;
import org.apache.tinkerpop.gremlin.structure.Graph;
import com.veadan.folib.gremlin.tx.GraphTransaction;
import com.veadan.folib.gremlin.tx.TransactionContext;
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
import org.springframework.data.repository.query.QueryLookupStrategy;


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
