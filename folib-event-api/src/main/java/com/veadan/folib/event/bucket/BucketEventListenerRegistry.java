package com.veadan.folib.event.bucket;

import com.veadan.folib.event.AbstractEventListenerRegistry;
import org.springframework.stereotype.Component;

@Component
public class BucketEventListenerRegistry extends AbstractEventListenerRegistry {


    public void dispatchUpdateBucketEvent(long capacity, long tokens) {
        BucketEvent event = new BucketEvent(capacity, tokens, BucketEventTypeEnum.UPDATE);
        dispatchEvent(event);
    }
}
