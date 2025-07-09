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
package com.folib.components;

import com.hazelcast.core.HazelcastInstance;
import com.folib.constant.GlobalConstants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
 * @date 2023/12/12
 **/
@Slf4j
@Component
public class DistributedCacheComponent {

    @Inject
    private HazelcastInstance hazelcastInstance;

    public void put(String cacheName, String cacheValue) {
        log.debug("Put cacheName [{}] cacheValue [{}]", cacheName, cacheValue);
        if (StringUtils.isNotBlank(cacheName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME).put(cacheName, cacheValue);
        }
    }

    public void put(String cacheName, String cacheValue, long ttl, TimeUnit timeUnit) {
        log.debug("Put cacheName [{}] cacheValue [{}] ttl [{}] timeUnit [{}]", cacheName, cacheValue, ttl, timeUnit);
        if (StringUtils.isNotBlank(cacheName)) {
            hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME).put(cacheName, cacheValue, ttl, timeUnit);
        }
    }

    public String get(String cacheName) {
        log.debug("Get cacheName [{}]", cacheName);
        try {
            Map<String, String> hazelcastMap = hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME);
            return hazelcastMap.get(cacheName);
        } catch (Exception ex) {
            log.warn(ExceptionUtils.getStackTrace(ex));
            return "";
        }
    }

    public void delete(String cacheName) {
        log.debug("Delete cacheName [{}]", cacheName);
        Map<String, String> hazelcastMap = hazelcastInstance.getMap(GlobalConstants.DISTRIBUTED_CACHE_NAME);
        hazelcastMap.remove(cacheName);
    }
}
