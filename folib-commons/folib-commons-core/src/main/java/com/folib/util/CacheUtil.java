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

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

import java.util.concurrent.TimeUnit;

/**
 * @param <K>
 * @param <V>
 * @author Veadan
 */
public class CacheUtil<K, V> {

    /**
     * 获取实例
     */
    private static final CacheUtil<?, ?> instance = new CacheUtil<>();

    /**
     * 缓存对象
     */
    private final Cache<K, V> cache;

    private CacheUtil() {
        this.cache = CacheBuilder.newBuilder()
                .expireAfterWrite(5, TimeUnit.MINUTES)
                .build();
    }

    public static <K, V> CacheUtil<K, V> getInstance() {
        @SuppressWarnings("unchecked")
        CacheUtil<K, V> typedInstance = (CacheUtil<K, V>) instance;
        return typedInstance;
    }

    /**
     * 添加缓存
     *
     * @param key
     * @param value
     */
    public void put(K key, V value) {
        cache.put(key, value);
    }

    /**
     * 获取缓存
     *
     * @param key
     * @return
     */
    public V get(K key) {
        return cache.getIfPresent(key);
    }

    /**
     * 更新缓存
     *
     * @param key
     * @param value
     */
    public void update(K key, V value) {
        cache.put(key, value);
    }

    /**
     * 移除缓存
     *
     * @param key
     */
    public void remove(K key) {
        cache.invalidate(key);
    }

    /**
     * 清除所有缓存
     */
    public void clearAll() {
        cache.invalidateAll();
    }
}

