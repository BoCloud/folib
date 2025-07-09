/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Proxy;
import java.net.URI;
import java.nio.channels.AsynchronousFileChannel;
import java.nio.channels.FileChannel;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.AccessMode;
import java.nio.file.CopyOption;
import java.nio.file.DirectoryStream;
import java.nio.file.DirectoryStream.Filter;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.LinkOption;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.FileAttributeView;
import java.nio.file.spi.FileSystemProvider;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;

/**
 * This {@link FileSystemProvider} implementation allows to have original
 * {@link Path} wrapped by {@link Proxy} wihout errors.
 * 
 * @author veadan
 *
 * @see ProxyPathInvocationHandler
 * @see ProxyPathFileSystem
 */
public class ProxyFileSystemProvider extends FileSystemProvider
{

    private final FileSystemProvider delegate;

    public ProxyFileSystemProvider(FileSystemProvider target)
    {
        this.delegate = target;
    }

    public int hashCode()
    {
        return delegate.hashCode();
    }

    public boolean equals(Object obj)
    {
        return delegate.equals(obj);
    }

    public String getScheme()
    {
        return delegate.getScheme();
    }

    public FileSystem newFileSystem(URI uri,
                                    Map<String, ?> env)
        throws IOException
    {
        return delegate.newFileSystem(uri, env);
    }

    public FileSystem getFileSystem(URI uri)
    {
        return delegate.getFileSystem(uri);
    }

    public String toString()
    {
        return delegate.toString();
    }

    public Path getPath(URI uri)
    {
        return delegate.getPath(uri);
    }

    public FileSystem newFileSystem(Path path,
                                    Map<String, ?> env)
        throws IOException
    {
        return delegate.newFileSystem(unwrap(path), env);
    }

    public InputStream newInputStream(Path path,
                                      OpenOption... options)
        throws IOException
    {
        return delegate.newInputStream(unwrap(path), options);
    }

    public OutputStream newOutputStream(Path path,
                                        OpenOption... options)
        throws IOException
    {
        return delegate.newOutputStream(unwrap(path), options);
    }

    public FileChannel newFileChannel(Path path,
                                      Set<? extends OpenOption> options,
                                      FileAttribute<?>... attrs)
        throws IOException
    {
        return delegate.newFileChannel(unwrap(path), options, attrs);
    }

    public AsynchronousFileChannel newAsynchronousFileChannel(Path path,
                                                              Set<? extends OpenOption> options,
                                                              ExecutorService executor,
                                                              FileAttribute<?>... attrs)
        throws IOException
    {
        return delegate.newAsynchronousFileChannel(unwrap(path), options, executor, attrs);
    }

    public SeekableByteChannel newByteChannel(Path path,
                                              Set<? extends OpenOption> options,
                                              FileAttribute<?>... attrs)
        throws IOException
    {
        return delegate.newByteChannel(unwrap(path), options, attrs);
    }

    public DirectoryStream<Path> newDirectoryStream(Path dir,
                                                    Filter<? super Path> filter)
        throws IOException
    {
        return delegate.newDirectoryStream(unwrap(dir), filter);
    }

    public void createDirectory(Path dir,
                                FileAttribute<?>... attrs)
        throws IOException
    {
        delegate.createDirectory(unwrap(dir), attrs);
    }

    public void createSymbolicLink(Path link,
                                   Path target,
                                   FileAttribute<?>... attrs)
        throws IOException
    {
        delegate.createSymbolicLink(unwrap(link), unwrap(target), attrs);
    }

    public void createLink(Path link,
                           Path existing)
        throws IOException
    {
        delegate.createLink(unwrap(link), unwrap(existing));
    }

    public void delete(Path path)
        throws IOException
    {
        delegate.delete(unwrap(path));
    }

    public boolean deleteIfExists(Path path)
        throws IOException
    {
        return delegate.deleteIfExists(unwrap(path));
    }

    public Path readSymbolicLink(Path link)
        throws IOException
    {
        return delegate.readSymbolicLink(unwrap(link));
    }

    public void copy(Path source,
                     Path target,
                     CopyOption... options)
        throws IOException
    {
        delegate.copy(unwrap(source), unwrap(target), options);
    }

    public void move(Path source,
                     Path target,
                     CopyOption... options)
        throws IOException
    {
        delegate.move(unwrap(source), unwrap(target), options);
    }

    public boolean isSameFile(Path path,
                              Path path2)
        throws IOException
    {
        return delegate.isSameFile(unwrap(path), unwrap(path2));
    }

    public boolean isHidden(Path path)
        throws IOException
    {
        return delegate.isHidden(unwrap(path));
    }

    public FileStore getFileStore(Path path)
        throws IOException
    {
        return delegate.getFileStore(unwrap(path));
    }

    public void checkAccess(Path path,
                            AccessMode... modes)
        throws IOException
    {
        delegate.checkAccess(unwrap(path), modes);
    }

    public <V extends FileAttributeView> V getFileAttributeView(Path path,
                                                                Class<V> type,
                                                                LinkOption... options)
    {
        return delegate.getFileAttributeView(unwrap(path), type, options);
    }

    public <A extends BasicFileAttributes> A readAttributes(Path path,
                                                            Class<A> type,
                                                            LinkOption... options)
        throws IOException
    {
        return delegate.readAttributes(unwrap(path), type, options);
    }

    public Map<String, Object> readAttributes(Path path,
                                              String attributes,
                                              LinkOption... options)
        throws IOException
    {
        return delegate.readAttributes(unwrap(path), attributes, options);
    }

    public void setAttribute(Path path,
                             String attribute,
                             Object value,
                             LinkOption... options)
        throws IOException
    {
        delegate.setAttribute(unwrap(path), attribute, value, options);
    }

    protected Path unwrap(Path path)
    {
        if (path == null)
        {
            return null;
        }
        else if (!Proxy.isProxyClass(path.getClass()))
        {
            return path;
        }

        return ((ProxyPathInvocationHandler) Proxy.getInvocationHandler(path)).getTarget();
    }
}
