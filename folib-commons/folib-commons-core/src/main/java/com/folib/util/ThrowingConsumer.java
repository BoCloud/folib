package com.folib.util;

import java.util.function.Consumer;

/**
 * @author Dawid Antecki
 */
@FunctionalInterface
public interface ThrowingConsumer<T, E extends Throwable>
{
    void accept(T t) throws E;

    static <T, E extends Throwable> Consumer<T> unchecked(ThrowingConsumer<T, E> consumer)
    {
        return t ->
        {
            try
            {
                consumer.accept(t);
            }
            catch (Throwable e)
            {
                throw new RuntimeException(e);
            }
        };
    }
}
