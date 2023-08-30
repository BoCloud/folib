package com.veadan.folib.util;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

import java.util.concurrent.TimeUnit;

public class CacheUtil<K, V> {
    private final Cache<K, V> cache;

    public CacheUtil() {
        this.cache = CacheBuilder.newBuilder()
                .expireAfterWrite(5, TimeUnit.MINUTES)  // 缓存项在给定时间内没有被写访问（创建或覆盖）后，将被自动移除
                .maximumSize(1000)  // 设置缓存的最大容量
                .build();
    }

    // 添加缓存
    public void put(K key, V value) {
        cache.put(key, value);
    }

    // 获取缓存
    public V get(K key) {
        return cache.getIfPresent(key);
    }

    // 更新缓存
    public void update(K key, V value) {
        cache.put(key, value);
    }

    // 移除缓存
    public void remove(K key) {
        cache.invalidate(key);
    }

    // 清除所有缓存
    public void clearAll() {
        cache.invalidateAll();
    }

    // 其他缓存操作...
}

