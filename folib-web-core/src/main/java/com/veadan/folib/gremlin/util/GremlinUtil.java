package com.veadan.folib.gremlin.util;

import org.apache.tinkerpop.gremlin.driver.Client;
import org.apache.tinkerpop.gremlin.driver.Cluster;
import org.apache.tinkerpop.gremlin.driver.MessageSerializer;
import org.apache.tinkerpop.gremlin.driver.remote.DriverRemoteConnection;
import org.apache.tinkerpop.gremlin.driver.ser.GryoMessageSerializerV3d0;
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;
import org.apache.tinkerpop.gremlin.structure.io.IoRegistry;
import org.apache.tinkerpop.gremlin.structure.io.gryo.GryoMapper;

import static org.apache.tinkerpop.gremlin.process.traversal.AnonymousTraversalSource.traversal;

/**
 * @Author: haifeng
 * @Date: 2019-09-03 22:49
 */
public class GremlinUtil {

    public static Cluster cluster(String host, int port, IoRegistry registry) {
        // GryoMapper.Builder builder = GryoMapper.build().addRegistry(JanusGraphIoRegistry.getInstance());
        GryoMapper.Builder builder = GryoMapper.build().addRegistry(registry);
        MessageSerializer serializer = new GryoMessageSerializerV3d0(builder);
        return Cluster.build().
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
