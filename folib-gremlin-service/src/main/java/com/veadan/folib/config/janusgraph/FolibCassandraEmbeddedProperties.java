package com.veadan.folib.config.janusgraph;

import org.folib.db.server.CassandraEmbeddedConfiguration;
import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.boot.context.properties.ConstructorBinding;
import org.folib.db.server.CassandraEmbeddedProperties;
import org.springframework.boot.context.properties.bind.ConstructorBinding;

@ConfigurationProperties(prefix = "folib.db.cassandra")
public class FolibCassandraEmbeddedProperties implements CassandraEmbeddedConfiguration {

    private String storageRoot;
    private String configLocation;

    public String getStorageRoot() {
        return storageRoot;
    }

    public void setStorageRoot(String storageRoot) {
        this.storageRoot = storageRoot;
    }

    public String getConfigLocation() {
        return configLocation;
    }

    public void setConfigLocation(String configLocation) {
        this.configLocation = configLocation;
    }
}

