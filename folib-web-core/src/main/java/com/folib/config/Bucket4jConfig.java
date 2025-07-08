package com.folib.config;


import com.folib.components.DistributedCacheComponent;
import com.folib.constant.GlobalConstants;
import io.github.bucket4j.Bandwidth;
import io.github.bucket4j.Bucket;
import io.github.bucket4j.Refill;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.time.Duration;
import java.util.concurrent.atomic.AtomicReference;

@Slf4j
@Configuration
public class Bucket4jConfig {

    private final AtomicReference<Bucket> bucketRef = new AtomicReference<>(createDefaultBucket());

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @PostConstruct
    public void init() {
        // 初始化时从缓存加载值
        loadBucketConfig();
    }

    public Bucket getBucket() {
        return bucketRef.get();
    }
    private void loadBucketConfig() {
        long capacity = getCapacity();
        long tokens = getTokens();
        refreshBucket(capacity, tokens);
    }
    public void refreshBucket(long capacity, long tokens) {
        Bandwidth bandwidth = Bandwidth.classic(capacity, Refill.greedy(tokens, Duration.ofSeconds(1)));
        Bucket newBucket = Bucket.builder().addLimit(bandwidth).build();
        bucketRef.set(newBucket);
    }

    private Bucket createDefaultBucket() {
        long defaultCapacity = getCapacity();
        long defaultTokens = getTokens();
        Bandwidth bandwidth = Bandwidth.classic(defaultCapacity, Refill.greedy(defaultTokens, Duration.ofSeconds(1)));
        return Bucket.builder().addLimit(bandwidth).build();
    }

    public long getCapacity() {
        long capacity = GlobalConstants.BUCKET_CAPACITY;
        if (distributedCacheComponent != null) {
            String cacheKey = distributedCacheComponent.get(GlobalConstants.BUCKET_CAPACITY_KEY);
            if (StringUtils.isNotBlank(cacheKey)) {
                capacity = Long.parseLong(cacheKey);
            }
        }
        log.info("getCapacity:{}", capacity);
        return capacity;
    }

    public long getTokens() {
        long tokens = GlobalConstants.BUCKET_TOKENS;
        if (distributedCacheComponent != null) {
            String cacheKey = distributedCacheComponent.get(GlobalConstants.BUCKET_TOKENS_KEY);
            if (StringUtils.isNotBlank(cacheKey)) {
                tokens = Long.parseLong(cacheKey);
            }
        }
        log.info("getTokens:{}", tokens);
        return tokens;
    }
}
