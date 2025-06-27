package com.veadan.folib.config.janusgraph;

import com.veadan.folib.db.schema.FolibSchema;
import org.janusgraph.core.JanusGraph;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Condition;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.folib.db.server.InMemoryJanusGraphServer;
import org.folib.db.server.JanusGraphConfiguration;
import org.folib.db.server.JanusGraphProperties;
import org.folib.db.server.JanusGraphServer;

/**
 * @author veadan
 */
@Configuration
@Conditional(InMemoryDbServerConfiguration.class)
public class InMemoryDbServerConfiguration implements Condition
{

    @Bean
    JanusGraphServer embeddedDbServer(DelegatingIdBlockQueueSupplier idBlockQueueSupplier, JanusGraphConfiguration janusGraphConfiguration)
    {
        return new InMemoryJanusGraphServer(janusGraphConfiguration, idBlockQueueSupplier);
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

        return profile.getName().equals(JanusGraphDbProfile.PROFILE_MEMORY);
    }

}
