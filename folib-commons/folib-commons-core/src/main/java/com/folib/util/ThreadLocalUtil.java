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

import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author veadan
 * @date 2023/12/5 21:36
 */
public class ThreadLocalUtil {
    private static final ThreadLocal<Map<String, Object>> THREAD_LOCAL = new ThreadLocal<>();
    
    public static void set(String key, Object value) {
        Map<String, Object> map = THREAD_LOCAL.get();
        if (null == map) {
            THREAD_LOCAL.set(new ConcurrentHashMap<>());
        }
        THREAD_LOCAL.get().put(key, value);
    }
    
    public static <T>T get(String key, Class<T> valueClass) {
        final Object o = THREAD_LOCAL.get().get(key);
        if (o != null) {
            return (T) o;
        }
        
        return null;
    }
}
