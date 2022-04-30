package com.veadan.folib.providers.io;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Set;

import com.veadan.folib.providers.layout.LayoutFileSystemProvider;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.io.StorageFileSystem;
import com.veadan.folib.storage.repository.Repository;

/**
 * This class decorates {@link StorageFileSystem} with common layout specific
 * logic. <br>
 * Root folder is the {@link Repository} base directory.
 *
 * @author Sergey Bespalov
 * 
 * @see Repository
 * @see LayoutProvider
 */
public abstract class LayoutFileSystem
        extends StorageFileSystem
{

    public static final String TRASH = ".trash";
    public static final String TEMP = ".temp";

    private final Repository repository;
    private final LayoutFileSystemProvider provider;
    
    public LayoutFileSystem(PropertiesBooter propertiesBooter,
                            Repository repository,
                            FileSystem storageFileSystem,
                            LayoutFileSystemProvider provider)
    {
        super(repository.getStorage(), propertiesBooter, storageFileSystem);
        this.repository = repository;
        this.provider = provider;
    }

    public Repository getRepository()
    {
        return repository;
    }

    @Override
    public LayoutFileSystemProvider provider()
    {
        return provider;
    }

    public void createRootDirectory() throws IOException
    {
        Path rootPath = resolveRootPath();
        Files.createDirectories(rootPath);
    }
    
    public void cleanupRootDirectory()
        throws IOException
    {
        Path storageRootPath = super.getRootDirectory();
        if (!Files.exists(storageRootPath) || !Files.isDirectory(storageRootPath))
        {
            return;
        }
        
        try (DirectoryStream<Path> dirStream = Files.newDirectoryStream(storageRootPath))
        {
            if (dirStream.iterator().hasNext())
            {
                return;
            }
        }
        Files.delete(storageRootPath);
    }
    
    public RootRepositoryPath getRootDirectory()
    {
        return new RootRepositoryPath(resolveRootPath(), this);
    }

    private Path resolveRootPath()
    {
        Path rootPath = Optional.ofNullable(repository.getBasedir())
                                .filter(p -> !p.trim().isEmpty())
                                .map(p -> getTarget().getPath(p).toAbsolutePath().normalize())
                                .orElseGet(() -> super.getRootDirectory().resolve(repository.getId()))
                                .toAbsolutePath()
                                .normalize();
        return rootPath;
    }

    public RepositoryPath getTrashPath()
    {
        return getRootDirectory().resolve(TRASH).toAbsolutePath();
    }

    protected RepositoryPath getTempPath()
    {
        return getRootDirectory().resolve(TEMP).toAbsolutePath();
    }

    public RepositoryPath getPath(String first,
                                  String... more)
    {
        return new RepositoryPath(getTarget().getPath(first, more), this);
    }

    public abstract Set<String> getDigestAlgorithmSet();

}
