package com.veadan.folib.config.janusgraph;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.folib.db.server.CassandraEmbeddedProperties;

@ConstructorBinding
@ConfigurationProperties(prefix = "folib.db.cassandra")
public class FolibCassandraEmbeddedProperties extends CassandraEmbeddedProperties
{

    public FolibCassandraEmbeddedProperties(String storageRoot,
                                                String configLocation)
    {
        super(storageRoot, configLocation);
    }

}
