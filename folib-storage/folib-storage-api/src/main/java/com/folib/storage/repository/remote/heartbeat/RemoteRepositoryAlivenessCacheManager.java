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
package com.folib.storage.repository.remote.heartbeat;

import java.util.Objects;

import jakarta.inject.Inject;

import org.apache.commons.lang3.BooleanUtils;
import com.folib.data.CacheName;
import com.folib.storage.repository.remote.RemoteRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.cache.Cache;
import org.springframework.cache.CacheManager;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class RemoteRepositoryAlivenessCacheManager
        implements RemoteRepositoryAlivenessService, DisposableBean
{

    private static final Logger logger = LoggerFactory.getLogger(RemoteRepositoryAlivenessCacheManager.class);
    
    private final Cache cache;

    @Inject
    RemoteRepositoryAlivenessCacheManager(CacheManager cacheManager)
    {
        cache = cacheManager.getCache(CacheName.Repository.REMOTE_REPOSITORY_ALIVENESS);
        Objects.requireNonNull(cache, "remoteRepositoryAliveness cache configuration was not provided");
    }

    @Override
    public boolean isAlive(RemoteRepository remoteRepository)
    {
        Boolean aliveness = cache.get(remoteRepository.getUrl(), Boolean.class);
        logger.trace("Remote repository [{}] aliveness cached value is [{}].",
                     remoteRepository.getUrl(),
                     aliveness);
        
        return BooleanUtils.isNotFalse(aliveness);
    }

    @Override
    public void put(RemoteRepository remoteRepository,
                    boolean aliveness)
    {
        logger.trace("Cache remote repository [{}] aliveness as [{}].",
                     remoteRepository.getUrl(),
                     aliveness);
        
        cache.put(remoteRepository.getUrl(), Boolean.valueOf(aliveness));
    }

    @Override
    public void destroy()
            throws Exception
    {
        logger.info("Destroy remote repository aliveness cache.");
        
        cache.clear();
    }
}
