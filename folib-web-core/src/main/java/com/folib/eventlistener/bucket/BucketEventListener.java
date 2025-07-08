package com.folib.eventlistener.bucket;

import com.folib.components.DistributedCacheComponent;
import com.folib.config.Bucket4jConfig;
import com.folib.constant.GlobalConstants;
import com.folib.event.AsyncEventListener;
import com.folib.event.bucket.BucketEvent;
import com.folib.event.bucket.BucketEventTypeEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.inject.Inject;

@Slf4j
@Component
public class BucketEventListener {

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Autowired
    private Bucket4jConfig bucket4jConfig;

    @AsyncEventListener
    public void handleEvent(BucketEvent event){
        if(BucketEventTypeEnum.UPDATE.equals(event.getBucketEventType())){
            bucket4jConfig.refreshBucket(getCapacity(event), getTokens(event));
        }
    }

    public long getCapacity(BucketEvent event) {
        long capacity = GlobalConstants.BUCKET_CAPACITY;
        if(event.getCapacity()!=null && event.getCapacity()>0){
            capacity = event.getCapacity();
        }
        String cacheKey = distributedCacheComponent.get(GlobalConstants.BUCKET_CAPACITY_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            capacity =  Long.parseLong(cacheKey);
        }
        log.info("getCapacity:{}", capacity);
        return capacity;
    }

    public long getTokens(BucketEvent event) {
        long tokens = GlobalConstants.BUCKET_TOKENS;
        if(event.getTokens()!=null && event.getTokens()>0){
            tokens = event.getTokens();
        }
        String cacheKey = distributedCacheComponent.get(GlobalConstants.BUCKET_TOKENS_KEY);
        if (StringUtils.isNotBlank(cacheKey)) {
            tokens =  Long.parseLong(cacheKey);
        }
        log.info("getTokens:{}", tokens);
        return tokens;
    }

}
