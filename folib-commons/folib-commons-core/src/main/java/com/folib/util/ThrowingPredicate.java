package com.folib.util;

import java.util.function.Predicate;

/**
 * @author Dawid Antecki
 */
@FunctionalInterface
public interface ThrowingPredicate<T, E extends Throwable>
{
    boolean test(T t) throws E;

    static<T, E extends Throwable> Predicate<T> unchecked(ThrowingPredicate<T, E> predicate)
    {
        return t ->
        {
            try
            {
                return predicate.test(t);
            }
            catch (Throwable e)
            {
                throw new RuntimeException(e);
            }
        };
    }
}
