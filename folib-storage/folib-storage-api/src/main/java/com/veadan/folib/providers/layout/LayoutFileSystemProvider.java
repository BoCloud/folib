package com.veadan.folib.providers.layout;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.spi.FileSystemProvider;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import javax.inject.Inject;

import org.carlspring.commons.io.reloading.FSReloadableInputStreamHandler;
import com.veadan.folib.artifact.ArtifactNotFoundException;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.event.repository.RepositoryEventListenerRegistry;
import com.veadan.folib.io.ByteRangeInputStream;
import com.veadan.folib.io.LayoutInputStream;
import com.veadan.folib.io.LayoutOutputStream;
import com.veadan.folib.io.LazyInputStream;
import com.veadan.folib.io.LazyOutputStream;
import com.veadan.folib.io.LazyOutputStream.OutputStreamSupplier;
import com.veadan.folib.io.StreamUtils;
import com.veadan.folib.providers.io.RepositoryFileAttributeType;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.StorageFileSystemProvider;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.ArtifactResolutionException;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class decorates {@link StorageFileSystemProvider} with common layout specific
 * logic. <br>
 *
 * @author xuxinping
 *
 * @see LayoutProvider
 */
public abstract class LayoutFileSystemProvider extends StorageFileSystemProvider
{

    private static final Logger logger = LoggerFactory.getLogger(LayoutFileSystemProvider.class);

    @Inject
    private ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Inject
    private ArtifactRepository artifactEntityRepository;


    public LayoutFileSystemProvider(FileSystemProvider storageFileSystemProvider)
    {
        super(storageFileSystemProvider);
    }

    protected abstract AbstractLayoutProvider getLayoutProvider();

    @Override
    public LazyInputStream newInputStream(Path path,
                                          OpenOption... options)
            throws IOException
    {
        return new LazyInputStream(() -> {
            try
            {
                if (!Files.exists(path))
                {
                    throw new ArtifactNotFoundException(path.toUri());
                }

                if (Files.isDirectory(path))
                {
                    throw new ArtifactNotFoundException(path.toUri(),
                                                        String.format("The artifact path is a directory: [%s]",
                                                                      path.toString()));
                }

                ByteRangeInputStream bris = new ByteRangeInputStream(super.newInputStream(path, options));
                bris.setReloadableInputStreamHandler(new FSReloadableInputStreamHandler(path));
                bris.setLength(Files.size(path));

                return decorateStream((RepositoryPath) path, bris);
            }
            catch (NoSuchAlgorithmException e)
            {
                throw new IOException(e);
            }
        });
    }

    protected LayoutInputStream decorateStream(RepositoryPath path,
                                               InputStream is)
            throws NoSuchAlgorithmException, IOException
    {
        // Add digest algorithm only if it is not a Checksum (we don't need a Checksum of Checksum).
        if (Boolean.TRUE.equals(RepositoryFiles.isChecksum(path)))
        {
            return new LayoutInputStream(is, Collections.emptySet());
        }

        return new LayoutInputStream(is, path.getFileSystem().getDigestAlgorithmSet());
    }

    public RepositoryPath getChecksumPath(RepositoryPath path,
                                          String digestAlgorithm)
    {
        String checksumExtension = ".".concat(digestAlgorithm.toLowerCase().replaceAll("-", ""));

        return path.resolveSibling(path.getFileName().toString().concat(checksumExtension));
    }

    @Override
    public LazyOutputStream newOutputStream(Path path,
                                            OpenOption... options)
            throws IOException
    {
        return new LazyOutputStream(() -> {
            if (Files.exists(path) && Files.isDirectory(path))
            {
                throw new ArtifactResolutionException(String.format("The artifact path is a directory: [%s]",
                                                                    path.toString()));
            }

            Files.createDirectories(path.getParent());

            try
            {
                return decorateStream((RepositoryPath) path, super.newOutputStream(path, options));
            }
            catch (NoSuchAlgorithmException e)
            {
                throw new IOException(e);
            }
        });
    }

    protected LayoutOutputStream decorateStream(RepositoryPath path,
                                                OutputStream os)
            throws NoSuchAlgorithmException, IOException
    {
        Set<String> digestAlgorithmSet = path.getFileSystem().getDigestAlgorithmSet();
        LayoutOutputStream result = new LayoutOutputStream(os);

        // Add digest algorithm only if it is not a Checksum (we don't need a Checksum of Checksum).
        if (Boolean.TRUE.equals(RepositoryFiles.isChecksum(path)))
        {
            return result;
        }

        digestAlgorithmSet.stream()
                          .forEach(e -> {
                              try
                              {
                                  result.addAlgorithm(e);
                              }
                              catch (NoSuchAlgorithmException t)
                              {
                                  logger.error("Digest algorithm not supported: alg-[{}]", e, t);
                              }
                          });
        return result;
    }

    public void storeChecksum(RepositoryPath basePath,
                              boolean forceRegeneration)
            throws IOException
    {
        Files.walk(basePath)
             .filter(p -> !Files.isDirectory(p))
             .filter(p -> {
                 try
                 {
                     return !Boolean.TRUE.equals(RepositoryFiles.isChecksum((RepositoryPath) p));
                 }
                 catch (IOException e)
                 {
                     logger.error("Failed to read attributes for [{}]", p, e);
                 }
                 return false;
             })
             .forEach(p -> {
                 try
                 {
                     writeChecksum((RepositoryPath) p, forceRegeneration);
                 }
                 catch (IOException e)
                 {
                     logger.error("Failed to write checksum for [{}]", p, e);
                 }
             });
    }


    protected void writeChecksum(RepositoryPath path,
                                 boolean force)
            throws IOException
    {
        try (InputStream is = newInputStream(path))
        {
            byte[] buffer = new byte[1024];
            while (is.read(buffer) > 0)
            {
                //calculate checksum while reading the stream
            }
            Set<String> digestAlgorithmSet = path.getFileSystem().getDigestAlgorithmSet();
            digestAlgorithmSet.stream()
                              .forEach(p ->
                                       {
                                           String checksum = StreamUtils.findSource(LayoutInputStream.class, is)
                                                                        .getMessageDigestAsHexadecimalString(p);
                                           RepositoryPath checksumPath = getChecksumPath(path, p);
                                           if (Files.exists(checksumPath) && !force)
                                           {
                                               return;
                                           }
                                           try
                                           {
                                               Files.write(checksumPath, checksum.getBytes());
                                           }
                                           catch (IOException e)
                                           {
                                               logger.error("Failed to write checksum for [{}]",
                                                            checksumPath.toString(), e);
                                           }
                                       });
        }
    }

    @Override
    public void delete(Path path,
                       boolean force)
            throws IOException
    {
        logger.debug("Deleting in ({})...", path);


        RepositoryPath repositoryPath = (RepositoryPath) path;
        deleteMetadata(repositoryPath);

        if (!Files.exists(path))
        {
            logger.warn("Path not found: path-[{}]", path);

            return;
        }

        boolean directory = Files.isDirectory(path);
        super.delete(path, force);
        if (!directory)
        {
            artifactEventListenerRegistry.dispatchArtifactPathDeletedEvent(path);
        }else{
            artifactEventListenerRegistry.dispatchArtifactDirectoryPathDeletedEvent(path);
        }

        logger.debug("Deleted [{}]", path);
    }
    @Override
    protected void doDeletePath(RepositoryPath repositoryPath,
                                boolean force)
            throws IOException
    {
        if (!RepositoryFiles.isArtifact(repositoryPath))
        {
            super.doDeletePath(repositoryPath, force);
            return;
        }

        Artifact artifactEntry = Optional.ofNullable(repositoryPath.getArtifactEntry())
                                         .orElseGet(() -> fetchArtifactEntry(repositoryPath));
        if (artifactEntry != null)
        {
            artifactEntityRepository.delete(artifactEntry);
        }

        super.doDeletePath(repositoryPath, force);
    }

    private Artifact fetchArtifactEntry(RepositoryPath repositoryPath)
    {
        Repository repository = repositoryPath.getRepository();
        String path;
        try
        {
            path = RepositoryFiles.relativizePath(repositoryPath);
        }
        catch (IOException e)
        {
            logger.error("Failed to fetch ArtifactEntry for [{}]", repositoryPath, e);
            return null;
        }

        return artifactEntityRepository.findOneArtifact(repository.getStorage().getId(),
                                                        repository.getId(),
                                                        path);
    }

    @Override
    public void deleteTrash(RepositoryPath path)
            throws IOException
    {
        Repository repository = path.getRepository();
        Storage storage = repository.getStorage();

        logger.debug("Emptying trash for {}:{}...", storage.getId(), repository.getId());

        super.deleteTrash(path);

        repositoryEventListenerRegistry.dispatchEmptyTrashEvent(storage.getId(), repository.getId());

        logger.debug("Trash for {}:{} removed.", storage.getId(), repository.getId());
    }



    @Override
    public void undelete(RepositoryPath path)
            throws IOException
    {
        Repository repository = path.getRepository();
        Storage storage = repository.getStorage();

        logger.debug("Attempting to restore: [{}]; ", path);

        super.undelete(path);

        repositoryEventListenerRegistry.dispatchUndeleteTrashEvent(storage.getId(), repository.getId());

        logger.debug("The trash for {}:{} has been undeleted.", storage.getId(), repository.getId());
    }

    @Override
    protected Map<RepositoryFileAttributeType, Object> getRepositoryFileAttributes(RepositoryPath repositoryRelativePath,
                                                                                   RepositoryFileAttributeType... attributeTypes)
            throws IOException
    {
        return getLayoutProvider().getRepositoryFileAttributes(repositoryRelativePath, attributeTypes);
    }

    protected void deleteMetadata(RepositoryPath repositoryPath)
            throws IOException
    {

    }

    public class PathOutputStreamSupplier implements OutputStreamSupplier
    {
        private Path path;

        private OpenOption[] options;

        public PathOutputStreamSupplier(Path path,
                                        OpenOption... options)
        {
            this.path = path;
            this.options = options;
        }

        @Override
        public OutputStream get() throws IOException
        {
            return LayoutFileSystemProvider.super.newOutputStream(unwrap(path), options);
        }

    }

}
