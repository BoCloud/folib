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
package com.folib.scanner.common.util;

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