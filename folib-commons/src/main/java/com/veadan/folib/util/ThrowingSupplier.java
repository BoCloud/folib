package com.veadan.folib.util;

import java.util.function.Supplier;

/**
 * @author Dawid Antecki
 */
@FunctionalInterface
public interface ThrowingSupplier<T, E extends Throwable>
{

    T get() throws E;

    static <T, E extends Throwable> Supplier<T> unchecked(ThrowingSupplier<T, E> supplier)
    {
        return () ->
        {
            try
            {
                return supplier.get();
            }
            catch (Throwable e)
            {
                throw new RuntimeException(e);
            }
        };
    }
}
