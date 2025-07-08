package com.folib.io;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.nio.file.Path;

/**
 * This {@link InvocationHandler} should be used to avoid errors when
 * {@link Path} wrapped by {@link Proxy}.
 * 
 * @author veadan
 * 
 * @see ProxyFileSystemProvider
 * @see ProxyPathFileSystem
 *
 */
public abstract class ProxyPathInvocationHandler implements InvocationHandler
{

    public abstract Path getTarget();

    @Override
    public Object invoke(Object proxy,
                         Method method,
                         Object[] args)
        throws Throwable
    {

        if ("getFileSystem".equals(method.getName()))
        {
            return new ProxyPathFileSystem(getTarget().getFileSystem());
        }

        try
        {
            return method.invoke(getTarget(), args);
        }
        catch (InvocationTargetException e)
        {
            throw e.getTargetException();
        }
    }

}
