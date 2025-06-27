package com.veadan.folib.io;

import java.io.IOException;
import java.lang.reflect.Proxy;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.WatchService;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.nio.file.spi.FileSystemProvider;
import java.util.Set;

/**
 * This {@link FileSystem} implementation allows to have original
 * {@link Path} wrapped by {@link Proxy} wihout errors.
 * 
 * @author veadan
 *
 * @see ProxyPathInvocationHandler
 * @see ProxyFileSystemProvider
 */
public class ProxyPathFileSystem extends FileSystem
{

    private final FileSystem target;

    public ProxyPathFileSystem(FileSystem target)
    {
        this.target = target;
    }

    public int hashCode()
    {
        return target.hashCode();
    }

    public boolean equals(Object obj)
    {
        return target.equals(obj);
    }

    public FileSystemProvider provider()
    {
        return new ProxyFileSystemProvider(target.provider());
    }

    public void close()
        throws IOException
    {
        target.close();
    }

    public boolean isOpen()
    {
        return target.isOpen();
    }

    public boolean isReadOnly()
    {
        return target.isReadOnly();
    }

    public String getSeparator()
    {
        return target.getSeparator();
    }

    public Iterable<Path> getRootDirectories()
    {
        return target.getRootDirectories();
    }

    public Iterable<FileStore> getFileStores()
    {
        return target.getFileStores();
    }

    public String toString()
    {
        return target.toString();
    }

    public Set<String> supportedFileAttributeViews()
    {
        return target.supportedFileAttributeViews();
    }

    public Path getPath(String first,
                        String... more)
    {
        return target.getPath(first, more);
    }

    public PathMatcher getPathMatcher(String syntaxAndPattern)
    {
        return target.getPathMatcher(syntaxAndPattern);
    }

    public UserPrincipalLookupService getUserPrincipalLookupService()
    {
        return target.getUserPrincipalLookupService();
    }

    public WatchService newWatchService()
        throws IOException
    {
        return target.newWatchService();
    }

}
