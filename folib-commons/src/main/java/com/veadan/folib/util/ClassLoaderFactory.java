package com.veadan.folib.util;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.springframework.util.Assert;

import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.Collection;

/**
 * @author Przemyslaw Fusik
 */
public final class ClassLoaderFactory
{

    private ClassLoaderFactory()
    {

    }

    @SuppressFBWarnings(value = "DP_CREATE_CLASSLOADER_INSIDE_DO_PRIVILEGED")
    public static URLClassLoader urlClassLoaderFromPaths(ClassLoader parent,
                                                         Collection<Path> paths)
    {
        Assert.notNull(paths, "paths collection cannot be null");

        final URL[] urls = paths.stream().map(Path::toUri)
                                         .map(ThrowingFunction.unchecked(URI::toURL))
                                         .toArray(URL[]::new);

        return new URLClassLoader(urls, parent);
    }
}
