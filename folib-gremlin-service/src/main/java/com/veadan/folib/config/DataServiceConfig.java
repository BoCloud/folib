package com.veadan.folib.config;

import com.veadan.folib.config.hazelcast.HazelcastConfiguration;
import com.veadan.folib.config.janusgraph.CommonDbServerConfiguration;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdaptersConfig;
import com.veadan.folib.gremlin.server.GremlinServerConfig;
import com.veadan.folib.repositories.RepositoriesConfig;
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
@ComponentScan({ "com.veadan.folib.data" })
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
