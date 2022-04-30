package com.veadan.folib.providers.io;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Proxy;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchEvent.Modifier;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.io.FilenameUtils;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.io.ProxyPathInvocationHandler;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.PathUtils;

/**
 * This implementation decorates storage {@link Path} implementation, which can be an "Cloud Path" or common
 * "File System Path".
 *
 * @author Sergey Bespalov
 *
 * @see RepositoryPathResolver
 *
 */
public class RepositoryPath
        implements Path
{

    private Path target;

    private LayoutFileSystem fileSystem;

    protected Artifact artifact;

    protected Map<RepositoryFileAttributeType, Object> cachedAttributes = new HashMap<>();

    protected URI uri;

    protected String path;

    public RepositoryPath(Path target,
                          LayoutFileSystem fileSystem)
    {
        this.target = target;
        this.fileSystem = fileSystem;
    }

    public String getStorageId()
    {
        return getRepository().getStorage().getId();
    }

    public String getRepositoryId()
    {
        return getRepository().getId();
    }
    
    protected Path getTarget()
    {
        return target;
    }

    public Artifact getArtifactEntry() throws IOException
    {
        return artifact;
    }

    public LayoutFileSystem getFileSystem()
    {
        return fileSystem;
    }

    public Repository getRepository()
    {
        return getFileSystem().getRepository();
    }

    public boolean isAbsolute()
    {
        return getTarget().isAbsolute();
    }

    public RepositoryPath getRoot()
    {
        return getFileSystem().getRootDirectory();
    }

    public Path getFileName()
    {
        return getTarget().getFileName();
    }

    public RepositoryPath getParent()
    {
        RepositoryPath parent = wrap(getTarget().getParent());

        validateParent(parent);

        return parent;
    }

    public int getNameCount()
    {
        return getTarget().getNameCount();
    }

    public RepositoryPath getName(int index)
    {
        return wrap(getTarget().getName(index));
    }

    public RepositoryPath subpath(int beginIndex,
                                  int endIndex)
    {
        return wrap(getTarget().subpath(beginIndex, endIndex));
    }

    public boolean startsWith(Path other)
    {
        return getTarget().startsWith(unwrap(other));
    }

    public boolean startsWith(String other)
    {
        return getTarget().startsWith(other);
    }

    public boolean endsWith(Path other)
    {
        return getTarget().endsWith(other);
    }

    public boolean endsWith(String other)
    {
        return getTarget().endsWith(other);
    }

    public RepositoryPath normalize()
    {
        return wrap(getTarget().normalize());
    }

    public RepositoryPath resolve(Path other)
    {
        if (other == null)
        {
            return this;
        }

        other = unwrap(other);

        validatePathRelativized(other);

        return wrap(getTarget().resolve(other));
    }

    public RepositoryPath resolve(String other)
    {
        if (other == null)
        {
            return this;
        }

        validateStringPathRelativized(other);

        return wrap(getTarget().resolve(other));
    }

    public RepositoryPath resolveSibling(Path other)
    {
        validatePathRelativized(other);

        other = unwrap(other);

        RepositoryPath result = wrap(getTarget().resolveSibling(other));

        validateSibling(result);

        return result;
    }

    protected Path unwrap(Path other)
    {
        other = other != null && Proxy.isProxyClass(other.getClass())
                ? ((ProxyPathInvocationHandler) Proxy.getInvocationHandler(other)).getTarget()
                : other;

        other = other instanceof RepositoryPath ? ((RepositoryPath)other).getTarget() : other;

        return other;
    }

    public RepositoryPath resolveSibling(String other)
    {
        validateStringPathRelativized(other);

        RepositoryPath result = wrap(getTarget().resolveSibling(other));

        validateSibling(result);

        return result;
    }

    public RepositoryPath relativize(Path other)
    {
        other = unwrap(other);

        return wrap(getTarget().relativize(other));
    }

    /**
     * Returns Path relative to Repository root.
     *
     * @return
     */
    public RepositoryPath relativize()
    {
        if (!isAbsolute())
        {
            return this;
        }

        RepositoryPath result = getFileSystem().getRootDirectory().relativize(this);
        if (result.startsWith(LayoutFileSystem.TRASH) || result.startsWith(LayoutFileSystem.TEMP))
        {
            result = result.subpath(1, result.getNameCount());
        }

        return result;
    }

    public URI toUri()
    {
        if (uri != null)
        {
            return uri;
        }

        Repository repository = getFileSystem().getRepository();
        Storage storage = repository.getStorage();
        try
        {
            uri = new URI(StorageFileSystemProvider.FOLIB_SCHEME,
                          null,
                          "/" + storage.getId() + "/" + repository.getId() + "/" +
                          FilenameUtils.separatorsToUnix(relativize().toString()),
                          null);
        }
        catch (URISyntaxException e)
        {
            uri = null;
        }
        return uri;
    }

    public RepositoryPath toAbsolutePath()
    {
        if (!isAbsolute())
        {
            RepositoryPath result = getFileSystem().getRootDirectory().resolve(this);
            result.artifact = this.artifact;

            return result;
        }

        return this;
    }

    public RepositoryPath toRealPath(LinkOption... options)
            throws IOException
    {
        return wrap(getTarget().toRealPath(options));
    }

    @Deprecated
    public File toFile()
    {
        return getTarget().toFile();
    }

    public WatchKey register(WatchService watcher,
                             Kind<?>[] events,
                             Modifier... modifiers)
            throws IOException
    {
        throw new UnsupportedOperationException();
    }

    public WatchKey register(WatchService watcher,
                             Kind<?>... events)
            throws IOException
    {
        throw new UnsupportedOperationException();
    }

    public Iterator<Path> iterator()
    {
        return getTarget().iterator();
    }

    public int compareTo(Path other)
    {
        return getTarget().compareTo(unwrap(other));
    }

    public RepositoryPath wrap(Path path)
    {
        return new RepositoryPath(path, fileSystem);
    }

    public String toString()
    {
        return getTarget().toString();
    }

    @Override
    public boolean equals(Object obj)
    {
        return  getTarget().equals(obj instanceof RepositoryPath ? unwrap((Path) obj) : obj);
    }

    @Override
    public int hashCode()
    {
        return getTarget().hashCode();
    }

    private void validatePathRelativized(final Path other)
    {
        if (!PathUtils.isRelativized(unwrap(target), unwrap(other)))
        {
            throw new RepositoryRelativePathConstructionException();
        }
    }

    private void validateStringPathRelativized(final String other)
    {
        if (!PathUtils.isRelativized(unwrap(target), other))
        {
            throw new RepositoryRelativePathConstructionException();
        }
    }

    private void validateParent(final Path parent)
    {
        RootRepositoryPath root = getFileSystem().getRootDirectory();
        if (parent.isAbsolute() && !parent.startsWith(root))
        {
            throw new RepositoryRelativePathConstructionException();
        }
    }

    private void validateSibling(final Path result)
    {
        final Path sibling = result;
        final String repositoryRootPath = getRoot().toString(); // String, intentionally
        if (sibling.isAbsolute() && !sibling.startsWith(repositoryRootPath))
        {
            throw new PathExceededRootRepositoryPathException();
        }
    }

}
