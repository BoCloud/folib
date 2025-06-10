package com.veadan.folib.util;

import java.util.function.Function;

/**
 * @author Dawid Antecki
 */
@FunctionalInterface
public interface ThrowingFunction<T, R>
{
    R apply(T t) throws Throwable;

    static <T, R> Function<T, R> unchecked(ThrowingFunction<T, R> function)
    {
        return t ->
        {
            try
            {
                return function.apply(t);
            }
            catch (Throwable e)
            {
                throw new RuntimeException(e);
            }
        };
    }
}
