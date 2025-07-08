package com.folib.gremlin.util;

import org.apache.tinkerpop.gremlin.driver.Client;
import org.apache.tinkerpop.gremlin.driver.Cluster;

import org.apache.tinkerpop.gremlin.driver.remote.DriverRemoteConnection;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;
import org.apache.tinkerpop.gremlin.structure.io.IoRegistry;
import org.apache.tinkerpop.gremlin.structure.io.graphson.GraphSONMapper;
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoMapper;
import org.apache.tinkerpop.gremlin.util.MessageSerializer;
import org.apache.tinkerpop.gremlin.util.ser.GraphSONMessageSerializerV3;

import static org.apache.tinkerpop.gremlin.process.traversal.AnonymousTraversalSource.traversal;

/**
 * @Author: haifeng
 * @Date: 2019-09-03 22:49
 */
public class GremlinUtil {

    public static Cluster cluster(String host, int port, IoRegistry registry) {
        // GryoMapper.Builder builder = GryoMapper.build().addRegistry(JanusGraphIoRegistry.getInstance());
        GraphSONMapper.Builder builder = GraphSONMapper.build().addRegistry(registry);
        MessageSerializer serializer = new GraphSONMessageSerializerV3(builder);
        return Cluster.build().maxContentLength(65536*10).
                addContactPoint(host).
                port(port).
                serializer(serializer).
                create();
    }


    public static Client client(Cluster cluster) {
        return cluster.connect().init();
    }

    public static GraphTraversalSource source(Cluster cluster) {
        return traversal().withRemote(DriverRemoteConnection.using(cluster, "g"));
    }
}
