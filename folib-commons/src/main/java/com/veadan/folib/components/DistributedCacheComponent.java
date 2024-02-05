package com.veadan.folib.components;

import com.hazelcast.core.HazelcastInstance;
import com.veadan.folib.constant.GlobalConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @author leipenghui
 * @date 2023/12/12
 **/
@Slf4j
@Component
public class DistributedCacheComponent {

    @Inject
    private HazelcastInstance hazelcastInstance;

    public void put(String cacheName, String cacheValue) {
        log.debug("Put cacheName [{}] cacheValue [{}]", cacheName, cacheValue);
        if (StringUtils.isNotBlank(cacheName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME).put(cacheName, cacheValue);
        }
    }

    public void put(String cacheName, String cacheValue, long ttl, TimeUnit timeUnit) {
        log.debug("Put cacheName [{}] cacheValue [{}] ttl [{}] timeUnit [{}]", cacheName, cacheValue, ttl, timeUnit);
        if (StringUtils.isNotBlank(cacheName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME).put(cacheName, cacheValue, ttl, timeUnit);
        }
    }

    public String get(String cacheName) {
        log.debug("Get cacheName [{}]", cacheName);
        try {
            Map<String, String> hazelcastMap = hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME);
            return hazelcastMap.get(cacheName);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return "";
        }
    }

    public void delete(String cacheName) {
        log.debug("Delete cacheName [{}]", cacheName);
        Map<String, String> hazelcastMap = hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME);
        hazelcastMap.remove(cacheName);
    }
}
