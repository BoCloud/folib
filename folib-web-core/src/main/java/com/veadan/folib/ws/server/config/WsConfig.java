package com.veadan.folib.ws.server.config;

import com.veadan.folib.components.DistributedCacheComponent;
import com.veadan.folib.constant.GlobalConstants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.concurrent.Semaphore;

@Component
public class WsConfig {


    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    public Semaphore getSemaphore() {

        return new Semaphore(getFoLibPromotionThread());
    }

    public int getFoLibPromotionThread() {
        int threadNumber = GlobalConstants.FOLIB_PROMOTION_THREAD;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.FOLIB_PROMOTION_THREAD_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            threadNumber = Integer.parseInt(cacheKey);
        }
        return threadNumber;
    }

    public int getWsRequestTimout() {
        int timeout = GlobalConstants.WS_REQUEST_TIMOUT;
        String cacheKey = distributedCacheComponent.get(GlobalConstants.WS_REQUEST_TIMOUT_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            timeout = Integer.parseInt(cacheKey);
        }
        return timeout;
    }
}
