package com.veadan.folib.storage.repository.remote.heartbeat;

import java.util.Objects;

import javax.inject.Inject;

import org.apache.commons.lang3.BooleanUtils;
import com.veadan.folib.data.CacheName;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
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

    public boolean isAlive(RemoteRepository remoteRepository)
    {
        Boolean aliveness = cache.get(remoteRepository.getUrl(), Boolean.class);
        logger.trace("Remote repository [{}] aliveness cached value is [{}].",
                     remoteRepository.getUrl(),
                     aliveness);
        
        return BooleanUtils.isNotFalse(aliveness);
    }

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
        logger.debug("Destroy remote repository aliveness cache.");
        
        cache.clear();
    }
}
