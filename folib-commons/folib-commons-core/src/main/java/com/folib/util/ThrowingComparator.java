package com.folib.util;

import java.util.Comparator;

/**
 * folib
 */
@FunctionalInterface
public interface ThrowingComparator<T, E extends Throwable>
{
    int compareTo(T t1, T t2) throws E;

    static<T, E extends Throwable> Comparator<T> unchecked(ThrowingComparator<T, E> comparator)
    {
        return (t1, t2) ->
        {
            try
            {
                return comparator.compareTo(t1, t2);
            }
            catch (Throwable e)
            {
                throw new RuntimeException(e);
            }
        };
    }
}
