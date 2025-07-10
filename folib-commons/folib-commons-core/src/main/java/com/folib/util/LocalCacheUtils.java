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
package com.folib.util;

import cn.hutool.cache.CacheUtil;
import cn.hutool.cache.impl.TimedCache;
import cn.hutool.core.date.DateUnit;

/**
 * @author veadan
 * @date 2022/12/2
 **/
public class LocalCacheUtils {

    /**
     * 默认缓存时长 单位s
     */
    private static final Long DEFAULT_TIMEOUT = 1 * 60 * 1000L;
    /**
     * 默认清理间隔时间 单位s
     */
    private static final Long CLEAN_TIMEOUT = 1 * 60 * 1000L;

    /**
     * 缓存对象
     */
    public static TimedCache<String, String> timedCache = CacheUtil.newTimedCache(DEFAULT_TIMEOUT);

    static {
        //启动定时任务
        timedCache.schedulePrune(CLEAN_TIMEOUT);
    }

    public static void put(String key, String value) {
        timedCache.put(key, value);
    }

    public static void put(String key, String value, Integer expire) {
        timedCache.put(key, value, DateUnit.SECOND.getMillis() * expire);
    }

    /**
     * 禁止延迟缓存 isUpdateLastAccess = false
     *
     * @param key                key
     * @param isUpdateLastAccess isUpdateLastAccess
     */
    public static String get(String key, boolean isUpdateLastAccess) {
        return timedCache.get(key, isUpdateLastAccess);
    }

    public static String get(String key) {
        return timedCache.get(key);
    }

    public static void remove(String key) {
        timedCache.remove(key);
    }

    public static void clear() {
        timedCache.clear();
    }
}
