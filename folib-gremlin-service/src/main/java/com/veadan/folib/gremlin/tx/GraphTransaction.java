package com.veadan.folib.gremlin.tx;

import java.util.Iterator;
import java.util.Optional;

import com.veadan.folib.repositories.TransactionalIdBlockQueueSuppiler;
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
 * @author xuxinping
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
