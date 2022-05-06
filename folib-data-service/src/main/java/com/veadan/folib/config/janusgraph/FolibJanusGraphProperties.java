package com.veadan.folib.config.janusgraph;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.folib.db.server.JanusGraphProperties;

@ConstructorBinding
@ConfigurationProperties(prefix = "folib.db.janusgraph")
public class FolibJanusGraphProperties extends JanusGraphProperties
{

    public FolibJanusGraphProperties(String configLocation)
    {
        super(configLocation);
    }
}
