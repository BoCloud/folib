package com.veadan.folib.providers.io;

import java.io.IOException;
import java.lang.reflect.Proxy;
import java.util.Objects;
import java.util.Optional;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class RepositoryPathResolver
{

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected ArtifactRepository artifactEntityRepository;

    @Inject
    protected RepositoryFileSystemRegistry fileSystemRegistry;

    public RootRepositoryPath resolve(String storageId,
                                      String repositoryId)
    {
        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        Objects.requireNonNull(storage, String.format("Storage [%s] not found", storageId));

        return resolve(storage.getRepository(repositoryId));
    }
    
    public RootRepositoryPath resolve(final Repository repository)
    {
        Objects.requireNonNull(repository, "Repository should be provided");

        LayoutFileSystemFactory fileSystemFactory = fileSystemRegistry.lookupRepositoryFileSystemFactory(repository);

        return fileSystemFactory.create(repository).getRootDirectory();
    }

    public RepositoryPath resolve(String storageId,
                                  String repositoryId,
                                  String path)
    {
        Storage storage = configurationManager.getConfiguration().getStorage(storageId);
        Objects.requireNonNull(storage, String.format("Storage [%s] not found", storageId));

        return resolve(storage.getRepository(repositoryId), path);
    }

    public RepositoryPath resolve(final Repository repository,
                                  final ArtifactCoordinates c)
    {
        return resolve(repository, c.buildPath());
    }

    public RepositoryPath resolve(final Repository repository,
                                  final RepositoryPath otherPath)
        throws IOException
    {
        if (otherPath.getRepository().getId().equals(repository.getId())
                && otherPath.getArtifactEntry() != null)
        {
            return otherPath;
        }

        return resolve(repository, RepositoryFiles.relativizePath(otherPath));
    }

    public RepositoryPath resolve(final Repository repository,
                                  final String path)
    {
        RootRepositoryPath repositoryPath = resolve(repository);
        if (repository.isGroupRepository())
        {
            return repositoryPath.resolve(path);
        }
        
        return new LazyRepositoryPath(repositoryPath.resolve(path));
    }

    public Artifact findOneArtifact(String storageId, String repositoryId, String path) {
        return artifactEntityRepository.findOneArtifact(storageId, repositoryId, path);
    }

    private class LazyRepositoryPath extends RepositoryPath
    {

        private LazyRepositoryPath(RepositoryPath target)
        {
            super(target.getTarget(), target.getFileSystem());
            this.artifact = target.artifact;
        }

        @Override
        public Artifact getArtifactEntry()
            throws IOException
        {
            Artifact artifactLocal = super.getArtifactEntry();
            if (artifactLocal == NullArtifact.INSTANCE)
            {
                return null;
            }
            else if (artifactLocal != null)
            {
                return artifactLocal;
            }

            if (this.getRepository().isGroupRepository() || !RepositoryFiles.isArtifact(this))
            {
                artifact = NullArtifact.INSTANCE;
            }
            else
            {
                artifact = Optional.ofNullable(artifactEntityRepository.findOneArtifact(getRepository().getStorage().getId(),
                                                                                        getRepository().getId(),
                                                                                        RepositoryFiles.relativizePath(this)))
                                   .orElse(NullArtifact.INSTANCE);
            }

            return getArtifactEntry();
            // TODO: we should check this restriction 
//            if (Files.exists(this) && !Files.isDirectory(this) && RepositoryFiles.isArtifact(this) && result == null)
//            {
//                throw new IOException(String.format("Corresponding [%s] record not found for path [%s]",
//                                                    ArtifactEntry.class.getSimpleName(), this));
//            }

        }

        @Override
        public RepositoryPath normalize()
        {
            RepositoryPath target = super.normalize();
            return new LazyRepositoryPath(target);
        }
        
    }
    
    private static class NullArtifact
    {

        private static final Artifact INSTANCE = (Artifact) Proxy.newProxyInstance(Artifact.class.getClassLoader(),
                                                                                   new Class[] { Artifact.class },
                                                                                   (proxy,
                                                                                    method,
                                                                                    args) -> {
                                                                                       throw new UnsupportedOperationException();
                                                                                   });

    }

}
