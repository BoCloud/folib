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
package com.folib.config.hazelcast;

import com.folib.data.CacheName;

import java.util.*;

import com.hazelcast.config.*;
//import com.hazelcast.config.EvictionConfig.MaxSizePolicy;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author veadan
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

    @Value("${cacheManagerConfiguration.tcpIpPort:5701}")
    public int tcpIpPort;

    @Value("${cacheManagerConfiguration.enableTcpIpConfig:false}")
    public boolean enableTcpIpConfig;


    @Value("${cacheManagerConfiguration.tcpIp.tcpIpTimeoutSeconds:10}")
    public int tcpIpTimeoutSeconds;


    @Value("#{'${cacheManagerConfiguration.tcpIp.members:}'.split(',')}")
    public String[] tcpIpMembers;


    public static MapConfig newDefaultMapConfig(String name,
                                                int maxSize,
                                                MaxSizePolicy maxSizePolicy,
                                                EvictionPolicy evictionPolicy)
    {
        return new MapConfig().setName(name)
                .setEvictionConfig(new EvictionConfig().setEvictionPolicy(evictionPolicy)
                        .setSize(maxSize)
                        .setMaxSizePolicy(maxSizePolicy));
    }

    public MapConfig authenticationCacheConfig(String name)
    {
        return new MapConfig().setName(name).setNearCacheConfig(new NearCacheConfig().setCacheLocalEntries(authenticationsCacheCacheLocalEntries)
                .setEvictionConfig(new EvictionConfig().setMaxSizePolicy(authenticationsCacheEvictionConfigMaxSizePolicy)
                        .setSize(authenticationsCacheEvictionConfigSize))
                .setInvalidateOnChange(authenticationsCacheInvalidateOnChange)
                .setTimeToLiveSeconds(authenticationsCacheInvalidateInterval));
    }

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
        //采用TCP/IP的方式，可以通过制定IP端口的方式，默认端口是5701可以通过参数指定；
//        config.setSecurityConfig(new SecurityConfig().setClientRealmConfig(groupConfigName, new RealmConfig().setUsernamePasswordIdentityConfig(groupConfigName, groupConfigPassword)));
        config.setClusterName(groupConfigName);
        config.getNetworkConfig().getJoin().getMulticastConfig().setEnabled(false);
        config.getNetworkConfig().setPort(tcpIpPort);
        config.getNetworkConfig().getJoin().getTcpIpConfig().setEnabled(enableTcpIpConfig);

        if(enableTcpIpConfig)
        {
            List<String> members = new ArrayList<>();
            Arrays.stream(tcpIpMembers).forEach(members::add);
            TcpIpConfig tcpIpConfig = config.getNetworkConfig().getJoin().getTcpIpConfig();
            tcpIpConfig.setMembers(members).setConnectionTimeoutSeconds(tcpIpTimeoutSeconds);
        }
        return config;
    }

}
