package com.veadan.folib.event.bucket;


import com.veadan.folib.event.Event;

public class BucketEvent extends Event {
    //生成令牌数量
    private Long tokens;
    //桶的容量
    private Long capacity;
    //操作类型
    private BucketEventTypeEnum bucketEventType;


    public BucketEvent(Long tokens, Long capacity, BucketEventTypeEnum bucketEventType) {
        super(bucketEventType);
        this.tokens = tokens;
        this.capacity = capacity;
        this.bucketEventType = bucketEventType;
    }

    public Long getCapacity() {
        return capacity;
    }

    public Long getTokens() {
        return tokens;
    }

    public BucketEventTypeEnum getBucketEventType() {
        return bucketEventType;
    }

    public void setBucketEventType(BucketEventTypeEnum bucketEventType) {
        this.bucketEventType = bucketEventType;
    }

    public void setCapacity(Long capacity) {
        this.capacity = capacity;
    }

    public void setTokens(Long tokens) {
        this.tokens = tokens;
    }
}
