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
package com.folib.config.janusgraph;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.JarURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import com.folib.db.schema.FolibSchema;
import org.janusgraph.core.JanusGraph;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.folib.db.server.CassandraEmbeddedConfiguration;
import org.folib.db.server.JanusGraphConfiguration;
import org.folib.db.server.JanusGraphServer;
import org.folib.db.server.JanusGraphWithEmbeddedCassandra;

/**
 * @author veadan
 * @author veadan
 */
@Configuration
@Conditional(EmbeddedDbServerConfiguration.class)
public class EmbeddedDbServerConfiguration implements Condition
{

    @Value("${folib.etc}")
    private String folibEtc;

    private static final Logger logger = LoggerFactory.getLogger(EmbeddedDbServerConfiguration.class);

    public static final String PATH_FOLIB_DB = "META-INF/com/veadan/folib/db";

    @Bean
    JanusGraphServer embeddedDbServer(DelegatingIdBlockQueueSupplier idBlockQueueSupplier,
                                      CassandraEmbeddedConfiguration cassandraConfiguration,
                                      JanusGraphConfiguration janusGraphConfiguration)
    {

//        if (!Files.exists(Paths.get(cassandraConfiguration.getStorageRoot())))
//        {
//            logger.info(String.format("Extract storage from [%s].", PATH_FOLIB_DB));
//            initStorage(cassandraConfiguration);
//            logger.info(String.format("Storage extracted to [%s].", cassandraConfiguration.getStorageRoot()));
//        }
        System.setProperty("folibEtc", folibEtc);
        return new JanusGraphWithEmbeddedCassandra(cassandraConfiguration, janusGraphConfiguration, idBlockQueueSupplier);
    }

    public JarFile getDbSchemaClasspathLocation()
        throws IOException
    {
        URL systemResource = JanusGraphWithEmbeddedCassandra.class.getResource("/" + PATH_FOLIB_DB);
        if (systemResource == null)
        {
            throw new IOException(String.format("Storage resource [%s] not found.", PATH_FOLIB_DB));
        }
        JarURLConnection connection = (JarURLConnection) systemResource.openConnection();

        return connection.getJarFile();
    }

    private void initStorage(CassandraEmbeddedConfiguration cassandraConfiguration)
    {
        try (JarFile jar = getDbSchemaClasspathLocation())
        {
            Enumeration<JarEntry> enumEntries = jar.entries();
            while (enumEntries.hasMoreElements())
            {
                JarEntry file = enumEntries.nextElement();
                if (!file.getName().startsWith(PATH_FOLIB_DB))
                {
                    continue;
                }

                Path filePath = Paths.get(cassandraConfiguration.getStorageRoot(),
                                          file.getName().replace(PATH_FOLIB_DB, ""));
                if (file.isDirectory())
                {
                    Files.createDirectories(filePath);
                    continue;
                }
                logger.info(String.format("从 [%s] 提取文件到 [%s].", file.getName(), filePath.toAbsolutePath().toString()));

                try (InputStream is = new BufferedInputStream(jar.getInputStream(file)))
                {
                    try (OutputStream os = new BufferedOutputStream(new java.io.FileOutputStream(filePath.toFile())))
                    {
                        while (is.available() > 0)
                        {
                            os.write(is.read());
                        }
                    }
                }
            }
        }
        catch (IOException e)
        {
            logger.warn(String.format("Failed to extract folib storage resource from [%s], reason [%s].",
                                      PATH_FOLIB_DB, e.getMessage()));
        }
    }

    @Bean
    JanusGraph JanusGraph(JanusGraphServer server)
        throws Exception
    {
        return new FolibSchema().createSchema(server.getJanusGraph());
    }

    @Override
    public boolean matches(ConditionContext conditionContext,
                           AnnotatedTypeMetadata metadata)

    {
        JanusGraphDbProfile profile = JanusGraphDbProfile.resolveProfile((ConfigurableEnvironment) conditionContext.getEnvironment());

        return profile.getName().equals(JanusGraphDbProfile.PROFILE_EMBEDDED);
    }

}
