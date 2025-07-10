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
package com.folib.utils;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

import java.util.*;

/**
 * @author veadan
 * @date 2024/2/18
 **/
public class MapUtil {

    /**
     * Map拆分 (指定分组大小)
     *
     * @param map       Map
     * @param chunkSize 每个分组的大小 (>=1)
     * @param <K>       Key
     * @param <V>       Value
     * @return 子Map列表
     */
    public static <K, V> List<Map<K, V>> splitByChunkSize(Map<K, V> map, int chunkSize) {
        if (Objects.isNull(map) || map.isEmpty() || chunkSize < 1) {
            //空map或者分组大小<1，无法拆分
            return Collections.emptyList();
        }
        //键值对总数
        int mapSize = map.size();
        //计算分组个数
        int groupSize = mapSize / chunkSize + (mapSize % chunkSize == 0 ? 0 : 1);
        //子Map列表
        List<Map<K, V>> list = Lists.newArrayListWithCapacity(groupSize);
        //只能分1组的情况
        if (chunkSize >= mapSize) {
            list.add(map);
            return list;
        }
        //每个分组的组内计数
        int count = 0;
        //子Map
        Map<K, V> subMap = Maps.newHashMapWithExpectedSize(chunkSize);

        for (Map.Entry<K, V> entry : map.entrySet()) {
            if (count < chunkSize) {
                //给每个分组放chunkSize个键值对，最后一个分组可能会装不满
                subMap.put(entry.getKey(), entry.getValue());
                //组内计数+1
                count++;
            } else {
                //结束上一个分组
                //当前分组装满了->加入列表
                list.add(subMap);

                //开始下一个分组
                //新的分组
                subMap = Maps.newHashMapWithExpectedSize(chunkSize);
                //添加当前键值对
                subMap.put(entry.getKey(), entry.getValue());
                //组内计数重置为1
                count = 1;
            }
        }
        //添加最后一个分组
        list.add(subMap);
        return list;
    }

    /**
     * Map拆分(指定分组个数)
     *
     * @param map       Map
     * @param groupSize 分组个数 (>=1)
     * @param <K>       Key
     * @param <V>       Value
     * @return 子Map列表
     */
    public static <K, V> List<Map<K, V>> splitByGroupSize(Map<K, V> map, int groupSize) {
        if (Objects.isNull(map) || map.isEmpty() || groupSize < 1) {
            //空map或者分组数<1，无法拆分
            return Collections.emptyList();
        }

        List<Map<K, V>> list = Lists.newArrayListWithCapacity(groupSize);
        //只有1个分组的情况
        if (groupSize == 1) {
            list.add(map);
            return list;
        }
        //键值对总数
        int mapSize = map.size();
        //当前分组的下标，[0, groupSize-1]
        int chunkIndex = 0;
        //平均后剩余的键值对数
        int restCount = mapSize % groupSize;
        //每个分组键值对数量
        int chunkSize0 = mapSize / groupSize;
        //多分一个
        int chunkSize1 = chunkSize0 + 1;
        //实际每组的大小（前面的部分分组可能会多分1个）
        int chunkSize = chunkIndex < restCount ? chunkSize1 : chunkSize0;
        //每个分组的组内计数
        int count = 0;
        //子Map
        Map<K, V> subMap = Maps.newHashMapWithExpectedSize(chunkSize);
        for (Map.Entry<K, V> entry : map.entrySet()) {
            if (count < chunkSize) {
                //每个分组按实际分组大小（chunkSize）加入键值对
                subMap.put(entry.getKey(), entry.getValue());
                //组内计数+1
                count++;
            } else {
                //结束上一个分组
                //当前分组装满了->加入列表
                list.add(subMap);
                //分组个数+1
                chunkIndex++;

                //开始下一个分组
                //重新计算分组大小
                chunkSize = chunkIndex < restCount ? chunkSize1 : chunkSize0;
                //新的分组
                subMap = Maps.newHashMapWithExpectedSize(chunkSize);
                //添加当前键值对
                subMap.put(entry.getKey(), entry.getValue());
                //组内计数重置为1
                count = 1;
            }
        }
        //添加最后一个分组
        list.add(subMap);
        return list;
    }
}