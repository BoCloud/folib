package com.folib.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CollectionUtils
{

    public static void putMapValue(String path,
                                   Object value,
                                   Map<String, Object> map)
    {
        if (path == null || path.trim().isEmpty())
        {
            return;
        }

        String[] split = path.split("\\.");
        Map<String, Object> targetMap = map;
        for (int i = 0; i < split.length - 1; i++)
        {
            String key = split[i];
            Object intermediateMap = (Map<String, Object>) targetMap.get(key);
            if (!(intermediateMap instanceof Map))
            {
                throw new IllegalArgumentException(String.format("No such property [%s].", key));
            }

            targetMap = (Map<String, Object>) intermediateMap;
        }

        targetMap.put(split[split.length - 1], value);
    }

    public static Object getMapValue(String path,
                                     Map<String, Object> map)
    {
        if (path == null || path.trim().isEmpty())
        {
            return map;
        }

        Object result = map;
        for (String property : path.split("\\."))
        {
            if (!(result instanceof Map))
            {
                throw new IllegalArgumentException(String.format("No such property [%s].", property));
            }

            result = ((Map<String, Object>) result).get(property);
        }

        return result;
    }

    public static Map<String, Object> flattenMap(Map<String, Object> map)
    {
        return flatten(map).collect(HashMap::new, flattenedEntryConsumer(null), HashMap::putAll);
    }

    public static Object flattenObject(Object o)
    {
        if (o instanceof Map)
        {
            return flattenMap((Map<String, Object>) o);
        }
        return o;
    }

    public static Stream<Entry<String, Object>> flatten(Map<String, Object> map)
    {
        return map.entrySet().stream().flatMap(CollectionUtils::flatten);
    }

    private static Stream<Entry<String, Object>> flatten(Entry<String, Object> entry)
    {
        String k = entry.getKey();
        Object v = entry.getValue();

        if (v instanceof Map)
        {
            return ((Map<String, Object>) v).entrySet()
                                            .stream()
                                            .flatMap(CollectionUtils::flatten)
                                            .collect(HashMap<String, Object>::new, flattenedEntryConsumer(k),
                                                     HashMap::putAll)
                                            .entrySet()
                                            .stream();
        }
        else if (v instanceof Collection)
        {
            AtomicInteger i = new AtomicInteger();
            Function<? super Object, ? extends String> flattenedKeyMapper = o -> k + "[" + i.getAndIncrement() + "]";

            return flatten(((Collection<Object>) v).stream()
                                                   .map(CollectionUtils::flattenObject)
                                                   .collect(Collectors.toMap(flattenedKeyMapper,
                                                                             CollectionUtils::flattenObject)));
        }

        return Stream.of(entry);
    }

    private static BiConsumer<HashMap<String, Object>, ? super Entry<String, Object>> flattenedEntryConsumer(String keyPrefix)
    {
        return (m, e) -> m.put(keyPrefix == null ? e.getKey() : keyPrefix + "." + e.getKey(), e.getValue());
    }

}
