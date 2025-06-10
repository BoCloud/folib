package com.veadan.folib.url;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

/**
 * @author mtodorov
 */
public class ClasspathURLStreamHandler extends URLStreamHandler
{

    private final ClassLoader classLoader;


    public ClasspathURLStreamHandler()
    {
        this.classLoader = getClass().getClassLoader();
    }

    public ClasspathURLStreamHandler(ClassLoader classLoader)
    {
        this.classLoader = classLoader;
    }

    @Override
    protected URLConnection openConnection(URL u)
            throws IOException
    {
        final URL resourceUrl = classLoader.getResource(u.getPath());
        return resourceUrl != null ? resourceUrl.openConnection() : null;
    }

}
