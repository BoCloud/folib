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

import com.folib.db.schema.FolibSchema;
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
