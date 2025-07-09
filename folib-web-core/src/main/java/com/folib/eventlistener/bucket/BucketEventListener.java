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
