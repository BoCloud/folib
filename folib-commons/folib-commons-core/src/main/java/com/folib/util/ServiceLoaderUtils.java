package com.folib.util;

import javax.annotation.Nonnull;
import java.util.Arrays;
import java.util.HashSet;
import java.util.ServiceLoader;
import java.util.Set;

/**
 * @author veadan
 */
public final class ServiceLoaderUtils
{

    @Nonnull
    public static Set<Class<?>> load(Class<?>... classes)
    {
        Set<Class<?>> contextClasses = new HashSet<>();
        if (classes != null)
        {
            Arrays.asList(classes).forEach(
                    clazz ->
                    {
                        ServiceLoader<?> loader = ServiceLoader.load(clazz);
                        loader.forEach(impl -> contextClasses.add(impl.getClass()));
                    }
            );
        }
        return contextClasses;
    }

}
