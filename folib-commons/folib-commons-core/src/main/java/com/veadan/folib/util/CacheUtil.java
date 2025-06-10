package com.veadan.folib.util;

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

