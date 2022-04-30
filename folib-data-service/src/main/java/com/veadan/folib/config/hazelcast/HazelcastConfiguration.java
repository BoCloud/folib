package com.veadan.folib.config.hazelcast;

import com.veadan.folib.data.CacheName;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import com.hazelcast.config.*;
import com.hazelcast.config.security.RealmConfig;
//import com.hazelcast.config.EvictionConfig.MaxSizePolicy;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author Przemyslaw Fusik
 */
@Configuration
public class HazelcastConfiguration
{

    @Value("${cacheManagerConfiguration.caches.authentications.timeToLiveSeconds:10}")
    public int authenticationsCacheInvalidateInterval;

    @Value("${cacheManagerConfiguration.caches.authentications.cacheLocalEntries:true}")
    public boolean authenticationsCacheCacheLocalEntries;

    @Value("${cacheManagerConfiguration.caches.authentications.evictionConfigSize:1000}")
    public int authenticationsCacheEvictionConfigSize;

    @Value("${cacheManagerConfiguration.caches.authentications.evictionConfigMaxSizePolicy:ENTRY_COUNT}")
    public MaxSizePolicy authenticationsCacheEvictionConfigMaxSizePolicy;

    @Value("${cacheManagerConfiguration.caches.authentications.invalidateOnChange:true}")
    public boolean authenticationsCacheInvalidateOnChange;

    public MapConfig authenticationCacheConfig(String name)
    {
        return new MapConfig().setName(name).setNearCacheConfig(new NearCacheConfig().setCacheLocalEntries(authenticationsCacheCacheLocalEntries)
                                                                                     .setEvictionConfig(new EvictionConfig().setMaxSizePolicy(authenticationsCacheEvictionConfigMaxSizePolicy)
                                                                                                                            .setSize(authenticationsCacheEvictionConfigSize))
                                                                                     .setInvalidateOnChange(authenticationsCacheInvalidateOnChange)
                                                                                     .setTimeToLiveSeconds(authenticationsCacheInvalidateInterval));
    }
    
    @Value("${cacheManagerConfiguration.caches.remoteRepositoryAliveness.maxSizeLimit:1000}")
    public int remoteRepositoryAlivenessMaxSizeLimit;

    @Value("${cacheManagerConfiguration.caches.remoteRepositoryAliveness.maxSizePolicy:FREE_HEAP_SIZE}")
    public MaxSizePolicy remoteRepositoryAlivenessMaxSizePolicy;

    @Value("${cacheManagerConfiguration.caches.remoteRepositoryAliveness.evictionPolicy:LFU}")
    public EvictionPolicy remoteRepositoryAlivenessEvictionPolicy;

    @Value("${cacheManagerConfiguration.caches.tags.maxSizeLimit:1000}")
    public int tagsMaxSizeLimit;

    @Value("${cacheManagerConfiguration.caches.tags.maxSizePolicy:FREE_HEAP_SIZE}")
    public MaxSizePolicy tagsMaxSizePolicy;

    @Value("${cacheManagerConfiguration.caches.tags.evictionPolicy:LFU}")
    public EvictionPolicy tagsEvictionPolicy;

    public static MapConfig newDefaultMapConfig(String name,
                                                int maxSize,
                                                MaxSizePolicy maxSizePolicy,
                                                EvictionPolicy evictionPolicy)
    {
        return new MapConfig().setName(name)
                              .setEvictionConfig(new EvictionConfig().setEvictionPolicy(evictionPolicy)
                                                                     .setMaxSizePolicy(maxSizePolicy));
    }

    @Bean
    public HazelcastInstance hazelcastInstance(Config config)
    {
        return Hazelcast.newHazelcastInstance(config);
    }

    @Value("${cacheManagerConfiguration.instanceId:folib}")
    public String hazelcastInstanceId;

    @Bean
    public HazelcastInstanceId hazelcastInstanceId()
    {
        return new HazelcastInstanceId(hazelcastInstanceId);
    }

    @Value("${cacheManagerConfiguration.groupConfig.name:folib}")
    public String groupConfigName;

    @Value("${cacheManagerConfiguration.groupConfig.password:password}")
    public String groupConfigPassword;

    @Value("${cacheManagerConfiguration.enableMulticastConfig:false}")
    public boolean enableMulticastConfig;

    @Value("${cacheManagerConfiguration.multicast.multicastGroup:224.2.2.3}")
    public String multicastGroup;

    @Value("${cacheManagerConfiguration.multicast.multicastPort:54327}")
    public int multicastPort;

    @Value("${cacheManagerConfiguration.multicast.multicastTimeoutSeconds:2}")
    public int multicastTimeoutSeconds;

    @Value("${cacheManagerConfiguration.multicast.multicastTimeToLive:32}")
    public int multicastTimeToLive;

    @Value("#{'${cacheManagerConfiguration.multicast.trustedInterfaces:}'.split(',')}")
    public String[] multicastTrustedInterfaces;

    @Value("${cacheManagerConfiguration.multicast.loopbackModeEnabled:false}")
    public boolean multicastLoopbackModeEnabled;

    @Bean
    public Config hazelcastConfig(HazelcastInstanceId hazelcastInstanceId)
    {
        final Config config = new Config().setInstanceName(hazelcastInstanceId.getInstanceName())
                                          .addMapConfig(newDefaultMapConfig(CacheName.Repository.REMOTE_REPOSITORY_ALIVENESS,
                                                                            remoteRepositoryAlivenessMaxSizeLimit,
                                                                            remoteRepositoryAlivenessMaxSizePolicy,
                                                                            remoteRepositoryAlivenessEvictionPolicy))
                                          .addMapConfig(newDefaultMapConfig(CacheName.Artifact.TAGS,
                                                                            tagsMaxSizeLimit,
                                                                            tagsMaxSizePolicy,
                                                                            tagsEvictionPolicy))
                                          .addMapConfig(authenticationCacheConfig(CacheName.User.AUTHENTICATIONS));
        //config.setGroupConfig(new GroupConfig(groupConfigName, groupConfigPassword));
        config.setSecurityConfig(new SecurityConfig().setClientRealmConfig(groupConfigName, new RealmConfig().setUsernamePasswordIdentityConfig(groupConfigName, groupConfigPassword)));
        config.setClusterName(groupConfigName);
        
        config.getNetworkConfig().getJoin().getMulticastConfig().setEnabled(enableMulticastConfig);

        if (enableMulticastConfig)
        {

            Set<String> trustedInterfaces = new HashSet<>();
            Arrays.stream(multicastTrustedInterfaces).forEach(trustedInterfaces::add);
            MulticastConfig mcConfig = config.getNetworkConfig().getJoin().getMulticastConfig();
            mcConfig.setMulticastGroup(multicastGroup)
                    .setMulticastPort(multicastPort)
                    .setMulticastTimeoutSeconds(multicastTimeoutSeconds)
                    .setMulticastTimeToLive(multicastTimeToLive)
                    .setTrustedInterfaces(trustedInterfaces)
                    .setLoopbackModeEnabled(multicastLoopbackModeEnabled);
        }

        return config;
    }

}
