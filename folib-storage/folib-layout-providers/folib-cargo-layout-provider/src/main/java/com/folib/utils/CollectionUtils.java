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


import com.google.common.collect.Multimap;
import java.util.Collection;
import java.util.Map;
import org.apache.commons.lang.StringUtils;

public class CollectionUtils {
    public static boolean isNotNullOrEmpty(Collection collection) {
        return (collection != null && !collection.isEmpty());
    }

    public static boolean isNullOrEmpty(Collection collection) {
        return (collection == null || collection.isEmpty());
    }

    public static Map<String, String> addNotNullAndBlankPropertyToMap(Map<String, String> propertiesMap, String key, Object value) {
        if (value != null && StringUtils.isNotBlank(value.toString()))
            propertiesMap.put(key, value.toString());
        return propertiesMap;
    }

    public static Map<String, String> addNotNullPropertyToMap(Map<String, String> propertiesMap, String key, Object value) {
        if (value != null)
            propertiesMap.put(key, value.toString());
        return propertiesMap;
    }

    public static void addNotNullPropertyToMultimap(Multimap<String, String> props, String key, Object... values) {
        if (values != null)
            for (Object value : values) {
                if (value != null)
                    props.put(key, value.toString());
            }
    }

    public static void addNonBlankPropertyToMultimap(Multimap<String, String> props, String key, Object... values) {
        if (values != null)
            for (Object value : values) {
                if (value != null && StringUtils.isNotBlank(value.toString()))
                    props.put(key, value.toString());
            }
    }
}

