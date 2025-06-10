package com.veadan.folib.util;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@SuppressWarnings("PMD.ClassNamingConventions")
public final class CustomStreamCollectors
{

    private CustomStreamCollectors()
    {

    }

    public static <T, K, U> Collector<T, ?, Map<K, U>> toLinkedHashMap(Function<? super T, ? extends K> keyMapper,
                                                                       Function<? super T, ? extends U> valueMapper)
    {
        return Collectors.toMap(
                keyMapper,
                valueMapper,
                (u, v) -> {
                    throw new IllegalStateException(String.format("Duplicate key %s", u));
                },
                LinkedHashMap::new);
    }

}
