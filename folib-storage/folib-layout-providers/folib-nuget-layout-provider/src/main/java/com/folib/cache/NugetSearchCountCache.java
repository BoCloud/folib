package com.folib.cache;

import com.hazelcast.config.MapConfig;
import com.hazelcast.core.HazelcastInstance;
import com.hazelcast.map.IMap;
import com.folib.nuget.filter.NugetSearchRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;



@Component
@Slf4j
public class NugetSearchCountCache {
    private final IMap<String, Integer> map;

    @Autowired
    public NugetSearchCountCache(@Qualifier("hazelcastInstance") HazelcastInstance hazelcastInstance) {
        String NUGET_REMOTE_CACHE_NAME = "nugetRemoteCache";
        this.map = hazelcastInstance.getMap(NUGET_REMOTE_CACHE_NAME);
        MapConfig mapConfig = hazelcastInstance.getConfig().getMapConfig(NUGET_REMOTE_CACHE_NAME);

        int AN_HOUR_AND_A_HALF = 90 * 60;
        mapConfig.setTimeToLiveSeconds(AN_HOUR_AND_A_HALF);
    }

    public void putV2(NugetSearchRequest searchRequest, int count) {
        String key = searchRequest.toCacheKeyV2();
        map.put(key, count);
    }

    public boolean containsV2(NugetSearchRequest searchRequest) {
        String key = searchRequest.toCacheKeyV2();
        return map.containsKey(key);
    }

    public int getV2(NugetSearchRequest searchRequest) {
        return map.get(searchRequest.toCacheKeyV2());
    }

    public void putV3(NugetSearchRequest searchRequest, int count) {
        String key = searchRequest.toCacheKeyV3();
        map.put(key, count);
    }

    public boolean containsV3(NugetSearchRequest searchRequest) {
        String key = searchRequest.toCacheKeyV3();
        return map.containsKey(key);
    }

    public int getV3(NugetSearchRequest searchRequest) {
        return map.get(searchRequest.toCacheKeyV3());
    }
}
