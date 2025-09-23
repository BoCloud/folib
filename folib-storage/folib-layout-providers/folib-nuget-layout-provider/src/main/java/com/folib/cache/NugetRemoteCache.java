package com.folib.cache;

import com.hazelcast.config.MapConfig;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;



@Slf4j
@Component
public class NugetRemoteCache {
    private final IMap<String, Boolean> map;

    @Autowired
    public NugetRemoteCache(@Qualifier("hazelcastInstance") HazelcastInstance hazelcastInstance) {
        String NUGET_REMOTE_CACHE_NAME = "nugetRemoteCache";
        this.map = hazelcastInstance.getMap(NUGET_REMOTE_CACHE_NAME);
        MapConfig mapConfig = hazelcastInstance.getConfig().getMapConfig(NUGET_REMOTE_CACHE_NAME);

        int AN_HOUR_AND_A_HALF = 90 * 60;
        mapConfig.setTimeToLiveSeconds(AN_HOUR_AND_A_HALF);
    }

    public void put(String path) {
        if (map.containsKey(path)) {
            log.warn("NugetRemoteCache already contains path: {}", path);
            return;
        }
        map.put(path, true);
        log.info("NugetRemoteCache put path: {}", path);
    }

    private boolean contains(String path) {
        return map.containsKey(path);
    }

    public boolean isActive(String path) {
        return map.containsKey(path);
    }
}
