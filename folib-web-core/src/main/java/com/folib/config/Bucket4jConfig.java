/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
