package com.veadan.folib.scanner.common.util;

import java.util.concurrent.ConcurrentHashMap;

public class MapCache {
    private static ConcurrentHashMap<String, String> cacheMap = new ConcurrentHashMap<>();

    /**
     * 获取缓存的对象
     *
     * @param account
     * @return
     */
    public static String getCache(String account) {

        // 如果缓冲中有该账号，则返回value
        if (cacheMap.containsKey(account)){
            return cacheMap.get(account);
        }

        return cacheMap.get(account);
    }

    /**
     * 初始化缓存
     *
     * @param account
     */
    public static void initCache(String account,String value) {
        // 一般是进行数据库查询，将查询的结果进行缓存
        cacheMap.put(account, value);
    }


    /**
     * 移除缓存信息
     *
     * @param account
     */
    public static void removeCache(String account) {
        if (cacheMap.containsKey(account)) {
            cacheMap.remove(account);
        }
    }
}