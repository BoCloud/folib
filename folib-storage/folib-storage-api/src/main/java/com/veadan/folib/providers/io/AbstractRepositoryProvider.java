package com.veadan.folib.providers.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.Optional;

import javax.inject.Inject;

import com.google.common.collect.Sets;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.io.*;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import org.apache.commons.io.input.CountingInputStream;
import org.apache.commons.io.output.CountingOutputStream;
import com.veadan.folib.artifact.ArtifactTag;
import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.data.criteria.Expression.ExpOperator;
import com.veadan.folib.data.criteria.Predicate;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactEntity;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.domain.ArtifactTagEntity;
import com.veadan.folib.event.artifact.ArtifactEventListenerRegistry;
import com.veadan.folib.io.*;
import com.veadan.folib.providers.repository.RepositoryProvider;
import com.veadan.folib.providers.repository.RepositoryProviderRegistry;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.repositories.ArtifactRepository;
import com.veadan.folib.services.ArtifactIdGroupService;
import com.veadan.folib.services.ArtifactTagService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.util.LocalDateTimeInstance;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.util.Assert;

/**
 * @author Veadan
 */
public abstract class AbstractRepositoryProvider implements RepositoryProvider, RepositoryStreamCallback
{

    private static final Logger logger = LoggerFactory.getLogger(AbstractRepositoryProvider.class);
    
    @Inject
    protected RepositoryProviderRegistry repositoryProviderRegistry;

    @Inject
    protected LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @Inject
    protected ArtifactRepository artifactRepository;
    
    @Inject
    private ArtifactIdGroupService artifactIdGroupService;
    
    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    
    @Inject
    protected ArtifactEventListenerRegistry artifactEventListenerRegistry;

    @Inject
    protected ApplicationEventPublisher eventPublisher;
    
    @Inject
    private RepositoryPathLock repositoryPathLock;
    
    @Inject
    private PlatformTransactionManager transactionManager;
    
    @Inject
    private ArtifactTagService artifactTagService;
    
    protected Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }
    
    @Override
    public RepositoryStreamSupport.RepositoryInputStream getInputStream(Path path)
        throws IOException
    {
        if (path == null)
        {
            return null;
        }
        Assert.isInstanceOf(RepositoryPath.class, path);
        RepositoryPath repositoryPath = (RepositoryPath) path;

        return decorate((RepositoryPath) path,
                        getInputStreamInternal(repositoryPath));

    }

    @Override
    public RepositoryStreamSupport.RepositoryStoreIndexInputStream getStoreIndexInputStream(Path path)
            throws IOException
    {
        if (path == null)
        {
            return null;
        }
        Assert.isInstanceOf(RepositoryPath.class, path);
        RepositoryPath repositoryPath = (RepositoryPath) path;

        return decorateIndex((RepositoryPath) path,
                getInputStreamInternal(repositoryPath));

    }

    protected abstract InputStream getInputStreamInternal(RepositoryPath repositoryPath)
        throws IOException;

    protected RepositoryStreamSupport.RepositoryInputStream decorate(RepositoryPath repositoryPath,
                                                                     InputStream is) throws IOException
    {
        if (is instanceof RepositoryStreamSupport.RepositoryInputStream)
        {
            return (RepositoryStreamSupport.RepositoryInputStream) is;
        }

        return new RepositoryStreamSupport(repositoryPathLock.lock(repositoryPath), this, transactionManager).
               new RepositoryInputStream(repositoryPath, is);
    }

    protected RepositoryStreamSupport.RepositoryStoreIndexInputStream decorateIndex(RepositoryPath repositoryPath,
                                                                     InputStream is) throws IOException
    {
        if (is instanceof RepositoryStreamSupport.RepositoryStoreIndexInputStream)
        {
            return (RepositoryStreamSupport.RepositoryStoreIndexInputStream) is;
        }

        return new RepositoryStreamSupport(repositoryPathLock.lock(repositoryPath), this, transactionManager).
                new RepositoryStoreIndexInputStream(repositoryPath, is);
    }

    @Override
    public RepositoryStreamSupport.RepositoryOutputStream getOutputStream(Path path)
        throws IOException
    {
        Assert.isInstanceOf(RepositoryPath.class, path);
        OutputStream os = getOutputStreamInternal((RepositoryPath) path);
        
        return decorate((RepositoryPath) path, os);
    }
    
    protected abstract OutputStream getOutputStreamInternal(RepositoryPath repositoryPath)
        throws IOException;

    protected final RepositoryStreamSupport.RepositoryOutputStream decorate(RepositoryPath repositoryPath,
                                                                            OutputStream os) throws IOException
    {
        if (os == null || os instanceof RepositoryStreamSupport.RepositoryOutputStream)
        {
            return (RepositoryStreamSupport.RepositoryOutputStream) os;
        }

        return new RepositoryStreamSupport(repositoryPathLock.lock(repositoryPath), this, transactionManager).
               new RepositoryOutputStream(repositoryPath, os);
    }

    @Override
    public void onBeforeWrite(RepositoryStreamWriteContext ctx) throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Writing [{}]", repositoryPath);
        
        if (!RepositoryFiles.isArtifact(repositoryPath))
        {
            return;
        }

        Repository repository = repositoryPath.getRepository();
        String storageId = repository.getStorage().getId();
        String repositoryId = repository.getId();

        Artifact artifactEntry = provideArtifact(repositoryPath);
        if (!shouldStoreArtifact(artifactEntry))
        {
            return;
        }
        
        artifactEntry.setStorageId(storageId);
        artifactEntry.setRepositoryId(repositoryId);

        LocalDateTime now = LocalDateTimeInstance.now();

        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);
        artifactEntry.setArtifactCoordinates(coordinates);
        artifactEntry.setCreated(now);
        artifactEntry.setLastUpdated(now);
        artifactEntry.setLastUsed(now);

        repositoryPath.artifact = artifactEntry;
    }

    @Override
    public void onAfterWrite(RepositoryStreamWriteContext ctx) throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Complete writing [{}]", repositoryPath);

        if (RepositoryFiles.isArtifact(repositoryPath))
        {
            if (ctx.getArtifactExists())
            {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(repositoryPath);
            }
            else
            {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(repositoryPath);
            }
        }
        else if (RepositoryFiles.isMetadata(repositoryPath))
        {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(repositoryPath);
        }
    }

    @Override
    public void onBeforeRead(RepositoryStreamReadContext ctx)
        throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Reading {}", repositoryPath);

        if (!RepositoryFiles.isArtifact(repositoryPath))
        {
            return;
        }
        
        artifactEventListenerRegistry.dispatchArtifactDownloadingEvent(repositoryPath);
    }
    
    @Override
    public void onAfterRead(RepositoryStreamReadContext ctx)
    {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();

        logger.debug("Complete reading [{}]", repositoryPath);
        
        artifactEventListenerRegistry.dispatchArtifactDownloadedEvent(repositoryPath);
    }

    @Override
    public void commit(RepositoryStreamWriteContext ctx) throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (artifact == null)
        {
            return;
        }

        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);
        
        CountingOutputStream cos = StreamUtils.findSource(CountingOutputStream.class, ctx.getStream());
        artifact.setSizeInBytes(cos.getByteCount());

        LayoutOutputStream los = StreamUtils.findSource(LayoutOutputStream.class, ctx.getStream());
        artifact.setChecksums(los.getDigestMap());
        
        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactsGroupWithTag(storage.getId(),
                                                                                            repository.getId(),
                                                                                            coordinates.getId(),
                                                                                            Optional.of(lastVersionTag))
                                                                 .orElseGet(() -> new ArtifactIdGroupEntity(storage.getId(),
                                                                                                            repository.getId(),
                                                                                                            coordinates.getId()));
        artifactGroup.setArtifacts(Sets.newHashSet());
        ArtifactCoordinates lastVersion = artifactIdGroupService.addArtifactToGroup(artifactGroup, artifact);
        logger.debug("Last version for group [{}] is [{}] with [{}]",
                     artifactGroup.getName(),
                     lastVersion.getVersion(),
                     lastVersion.getPath());
        
        artifactIdGroupRepository.merge(artifactGroup);
    }

    @Override
    public void commitStoreIndex(RepositoryStreamReadContext ctx) throws IOException
    {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        Artifact artifact = repositoryPath.getArtifactEntry();
        if (artifact == null) {
            artifact = provideArtifact(repositoryPath);
            if (artifact == null)
            {
                return;
            }
        }
        if (!shouldStoreArtifact(artifact))
        {
            return;
        }
        LocalDateTime now = LocalDateTimeInstance.now();
        artifact.setCreated(now);
        artifact.setLastUpdated(now);
        artifact.setLastUsed(now);
        Repository repository = repositoryPath.getRepository();
        Storage storage = repository.getStorage();
        ArtifactCoordinates coordinates = RepositoryFiles.readCoordinates(repositoryPath);

        artifact.setSizeInBytes(Files.size(repositoryPath));

        LayoutInputStream lis = StreamUtils.findSource(LayoutInputStream.class, ctx.getStream());
        artifact.setChecksums(lis.getDigestMap());

        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);

        ArtifactIdGroup artifactGroup = artifactIdGroupRepository.findArtifactsGroupWithTag(storage.getId(),
                repository.getId(),
                coordinates.getId(),
                Optional.of(lastVersionTag))
                .orElseGet(() -> new ArtifactIdGroupEntity(storage.getId(),
                        repository.getId(),
                        coordinates.getId()));
        artifactGroup.setArtifacts(Sets.newHashSet());
        ArtifactCoordinates lastVersion = artifactIdGroupService.addArtifactToGroup(artifactGroup, artifact);
        logger.debug("Last version for group [{}] is [{}] with [{}]",
                artifactGroup.getName(),
                lastVersion.getVersion(),
                lastVersion.getPath());

        artifactIdGroupRepository.merge(artifactGroup);

        repositoryPath.artifact = artifact;
    }

    @Override
    public void onStoreIndexAfter(RepositoryStreamReadContext ctx) throws IOException {
        RepositoryPath repositoryPath = (RepositoryPath) ctx.getPath();
        logger.debug("Complete build index [{}]", repositoryPath);

        if (RepositoryFiles.isArtifact(repositoryPath))
        {
            if (ctx.getArtifactExists())
            {
                artifactEventListenerRegistry.dispatchArtifactUpdatedEvent(repositoryPath);
            }
            else
            {
                artifactEventListenerRegistry.dispatchArtifactStoredEvent(repositoryPath);
            }
        }
        else if (RepositoryFiles.isMetadata(repositoryPath))
        {
            artifactEventListenerRegistry.dispatchArtifactMetadataStoredEvent(repositoryPath);
        }
    }

    protected Artifact provideArtifact(RepositoryPath repositoryPath) throws IOException
    {
        return Optional.ofNullable(repositoryPath.getArtifactEntry())
                       .orElse(new ArtifactEntity(repositoryPath.getStorageId(), repositoryPath.getRepositoryId(),
                               RepositoryFiles.readCoordinates(repositoryPath)));
    }
    
    protected boolean shouldStoreArtifact(Artifact artifactEntry)
    {
        return artifactEntry.getNativeId() == null;
    }
    
    @Override
    public RepositoryPath fetchPath(Path repositoryPath)
            throws IOException
    {
        return fetchPath((RepositoryPath)repositoryPath);
    }

    protected abstract RepositoryPath fetchPath(RepositoryPath repositoryPath) throws IOException;
    
    protected Predicate createPredicate(String storageId,
                                        String repositoryId,
                                        Predicate predicate)
    {
        Predicate result = Predicate.of(ExpOperator.EQ.of("storageId", storageId))
                                    .and(Predicate.of(ExpOperator.EQ.of("repositoryId", repositoryId)));
        if (predicate.isEmpty())
        {
            return result;
        }

        return result.and(predicate);
    }

    protected Selector<ArtifactEntity> createSelector(String storageId,
                                                      String repositoryId,
                                                      Predicate p)
    {
        Selector<ArtifactEntity> selector = new Selector<>(ArtifactEntity.class);
        selector.where(createPredicate(storageId, repositoryId, p));
        
        return selector;
    }

}
