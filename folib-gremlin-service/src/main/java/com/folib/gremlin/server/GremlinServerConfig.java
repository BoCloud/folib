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
package com.folib.gremlin.server;

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
