package com.veadan.folib.config.janusgraph;

import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.boot.context.properties.ConstructorBinding;
import org.folib.db.server.JanusGraphProperties;
import org.springframework.boot.context.properties.bind.ConstructorBinding;


@ConfigurationProperties(prefix = "folib.db.janusgraph")
public class FolibJanusGraphProperties extends JanusGraphProperties {

    private String configLocation;

    public FolibJanusGraphProperties() {
        super(null); // 或者在 setConfigLocation 里调用 super
    }

    public String getConfigLocation() {
        return configLocation;
    }

    public void setConfigLocation(String configLocation) {
        this.configLocation = configLocation;
    }
}

