package com.veadan.folib.repositories;

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
 * @author sbespalov
 */
public class TransactionalIdBlockQueueSuppiler implements Supplier<IdBlockQueueSession>
{

    private static final String[] PACKAGES = new String[] { "com.veadan.folib.domain",
                                                            "com.veadan.folib.artifact.coordinates" };

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
