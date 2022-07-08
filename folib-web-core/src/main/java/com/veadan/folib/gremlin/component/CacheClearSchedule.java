package com.veadan.folib.gremlin.component;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @Author: haifeng
 * @Date: 2019-09-10 11:10
 */
@Component
public class CacheClearSchedule {

    @Autowired
    private ClusterCache clusterCache;

    @Scheduled(cron = "0 */30 * * * *")
    public void clearCache() {
        clusterCache.clear(30 * 60);
    }
}
