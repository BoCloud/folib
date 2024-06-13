package com.veadan.folib.domain.huggingface.utils;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import javax.annotation.Nullable;
import org.apache.commons.lang.StringUtils;

public class CollectionUtils {
    public static boolean isNotNullOrEmpty(Collection collection) {
        return (collection != null && !collection.isEmpty());
    }

    public static boolean isNullOrEmpty(Collection collection) {
        return (collection == null || collection.isEmpty());
    }

    public static void addNotNullAndBlankPropertyToMultimap(Multimap<String, String> props, String key, Object value) {
        if (value != null && StringUtils.isNotBlank(value.toString())) {
            props.put(key, value.toString());
        }
    }

    public static void addNotNullMultiValuePropertyToMultimap(Multimap<String, String> props, String key, Object... values) {
        if (values != null) {
            for (Object value : values) {
                if (value != null) {
                    props.put(key, value.toString());
                }
            }
        }
    }

    @Nullable
    public static String getSingleValueProperty(@Nullable Multimap<String, String> props, String key) {
        if (props != null) {
            Collection<String> values = props.get(key);
            if (values != null) {
                return values.stream().filter(Objects::nonNull).findFirst().orElse(null);
            }
        }
        return null;
    }

    public static Multimap<String, String> createMultimapFromSingleKey(String propKey, Collection<String> propValues) {
        HashMultimap hashMultimap = HashMultimap.create();
        if (propValues != null) {
            addNotNullMultiValuePropertyToMultimap((Multimap<String, String>) hashMultimap, propKey, propValues.toArray());
        }
        return (Multimap<String, String>)hashMultimap;
    }

    public static HashMultimap<String, String> createMultimapFromPropsMap(Map<String, Collection<String>> props) {
        HashMultimap<String, String> multimap = HashMultimap.create();
        for (String key : props.keySet()) {
            multimap.putAll(key, props.get(key));
        }
        return multimap;
    }
}


