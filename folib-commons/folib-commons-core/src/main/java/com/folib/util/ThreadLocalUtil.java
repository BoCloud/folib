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
