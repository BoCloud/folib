package com.veadan.folib.config.janusgraph;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.folib.db.server.JanusGraphProperties;

@ConstructorBinding
@ConfigurationProperties(prefix = "folib.db.janusgraph")
public class StrongboxJanusGraphProperties extends JanusGraphProperties
{

    public StrongboxJanusGraphProperties(String configLocation)
    {
        super(configLocation);
    }
}
