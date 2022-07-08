package com.veadan.folib.gremlin.server;

import java.io.InputStream;

import org.apache.tinkerpop.gremlin.server.GremlinServer;
import org.apache.tinkerpop.gremlin.server.Settings;
import org.janusgraph.core.JanusGraph;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.Resource;

/**
 * @author veadan
 * @author xuxinping
 */
@Configuration
@ConditionalOnProperty(prefix = "folib.graph.gremlin.server", name = "enabled", havingValue = "true")
public class GremlinServerConfig
{

    @Bean(destroyMethod = "stop")
    GremlinServer gremlinServer(JanusGraph janusGraph,
                                @Value("classpath:/gremlin/gremlin-server.yaml")
                                Resource gremlinServerConf)
            throws Exception
    {
        GremlinServer server;
        try (InputStream inputStream = gremlinServerConf.getInputStream())
        {
            Settings settings = Settings.read(inputStream);
            server = new GremlinServer(settings);
        }

        server.getServerGremlinExecutor().getGraphManager().putGraph("graph", janusGraph);
        server.getServerGremlinExecutor().getGremlinExecutor().getScriptEngineManager().put("g", janusGraph.traversal());

        server.start().join();
        return server;
    }

}
