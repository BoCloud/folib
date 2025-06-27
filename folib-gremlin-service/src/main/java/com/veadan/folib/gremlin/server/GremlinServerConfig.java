package com.veadan.folib.gremlin.server;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import org.apache.tinkerpop.gremlin.server.GremlinServer;
import org.apache.tinkerpop.gremlin.server.Settings;
import org.janusgraph.core.JanusGraph;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;
import org.springframework.core.io.Resource;

import javax.annotation.PostConstruct;

/**
 * @author veadan
 * @author veadan
 */
@Configuration
@ConditionalOnProperty(prefix = "folib.graph.gremlin.server", name = "enabled", havingValue = "true")
public class GremlinServerConfig
{

    @Autowired
    private Environment environment;
    @Bean(destroyMethod = "stop")
    GremlinServer gremlinServer(JanusGraph janusGraph,
                                @Value("classpath:/gremlin/gremlin-server.yaml")
                                Resource gremlinServerConf)
            throws Exception
    {
        GremlinServer server;
        try (InputStream inputStream = gremlinServerConf.getInputStream())
        {
            //YAML 文件内容
            String yamlContent = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
            // 使用 Spring 环境解析占位符
            yamlContent = environment.resolvePlaceholders(yamlContent);

            // 将解析后的 YAML 内容传递给 Settings
            try (ByteArrayInputStream is = new ByteArrayInputStream(yamlContent.getBytes())){
                Settings settings = Settings.read(is);
                server = new GremlinServer(settings);
            }

        }

        server.getServerGremlinExecutor().getGraphManager().putGraph("graph", janusGraph);
        server.getServerGremlinExecutor().getGremlinExecutor().getScriptEngineManager().put("g", janusGraph.traversal());

        server.start().join();
        return server;
    }
}
