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

