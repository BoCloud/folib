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
package com.folib.config;

import com.folib.config.hazelcast.HazelcastConfiguration;
import com.folib.config.janusgraph.CommonDbServerConfiguration;
import com.folib.gremlin.adapters.EntityTraversalAdaptersConfig;
import com.folib.gremlin.server.GremlinServerConfig;
import com.folib.repositories.RepositoriesConfig;
import org.springframework.boot.context.properties.ConfigurationPropertiesScan;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.Lazy;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.spring.cache.HazelcastCacheManager;

/**
 * Spring configuration for data service project.
 *
 * @author 
 * @author veadan
 * @author veadan
 */
@Configuration
@Lazy(false)
@EnableConfigurationProperties
@ConfigurationPropertiesScan
@EnableTransactionManagement(proxyTargetClass = true, order = DataServiceConfig.TRANSACTIONAL_INTERCEPTOR_ORDER)
@EnableAspectJAutoProxy(proxyTargetClass = true)
@ComponentScan({ "com.folib.data" })
@Import({ CommonDbServerConfiguration.class,
          GremlinServerConfig.class,
          RepositoriesConfig.class,
          EntityTraversalAdaptersConfig.class,
          HazelcastConfiguration.class})
@EnableCaching(order = 105)
public class DataServiceConfig
{

    /**
     * This must be after {@link OEntityUnproxyAspect} order.
     */
    public static final int TRANSACTIONAL_INTERCEPTOR_ORDER = 100;

    @Bean
    public CacheManager cacheManager(HazelcastInstance hazelcastInstance)
    {
        return new HazelcastCacheManager(hazelcastInstance);
    }

}
