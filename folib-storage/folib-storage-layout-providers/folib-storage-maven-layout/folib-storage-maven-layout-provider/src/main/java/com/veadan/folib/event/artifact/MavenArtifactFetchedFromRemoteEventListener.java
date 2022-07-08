package com.veadan.folib.event.artifact;

import com.veadan.folib.artifact.MavenArtifact;
import com.veadan.folib.artifact.MavenArtifactUtils;
import com.veadan.folib.client.RestArtifactResolver;
import com.veadan.folib.event.AsyncEventListener;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathLock;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryArtifactResolver;
import com.veadan.folib.providers.repository.proxied.ProxyRepositoryInputStream;
import com.veadan.folib.providers.repository.proxied.RestArtifactResolverFactory;
import com.veadan.folib.providers.ProviderImplementationException;
import com.veadan.folib.storage.metadata.MetadataHelper;
import com.veadan.folib.storage.metadata.MetadataType;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;

import javax.inject.Inject;
import java.io.BufferedInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.locks.Lock;

import org.apache.maven.artifact.repository.metadata.Metadata;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenArtifactFetchedFromRemoteEventListener
        extends BaseMavenArtifactEventListener
{

    @Inject
    private ProxyRepositoryArtifactResolver proxyRepositoryArtifactResolver;

    @Inject
    protected RepositoryPathLock repositoryPathLock;
    
    @Inject
    protected RestArtifactResolverFactory restArtifactResolverFactory;
    
    @AsyncEventListener
    public void handle(final ArtifactEvent<RepositoryPath> event)
    {
        final Repository repository = getRepository(event);

        if (!Maven2LayoutProvider.ALIAS.equals(repository.getLayout()))
        {
            return;
        }

        if (event.getType() != ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_FETCHED_FROM_REMOTE.getType())
        {
            return;
        }

        resolveArtifactMetadataAtArtifactIdLevel(event);
        updateMetadataInGroupsContainingRepository(event, path -> path.getParent().getParent());
    }

    private void resolveArtifactMetadataAtArtifactIdLevel(final ArtifactEvent<RepositoryPath> event)
    {
        try
        {
            final RepositoryPath artifactAbsolutePath = event.getPath().toAbsolutePath();
            final RepositoryPath artifactBaseAbsolutePath = artifactAbsolutePath.getParent();

            final RepositoryPath metadataAbsolutePath = (RepositoryPath) MetadataHelper.getMetadataPath(
                    artifactBaseAbsolutePath,
                    null,
                    MetadataType.PLUGIN_GROUP_LEVEL);
            try
            {
                mavenMetadataManager.readMetadata(artifactBaseAbsolutePath.getParent());
            }
            catch (final FileNotFoundException ex)
            {
                downloadArtifactMetadataAtArtifactIdLevelFromRemote(metadataAbsolutePath);
                return;
            }

            downloadArtifactMetadataAtArtifactIdLevelFromRemoteAndMergeWithLocal(artifactAbsolutePath,
                                                                                 metadataAbsolutePath);
        }
        catch (Exception e)
        {
            logger.error("Unable to resolve artifact metadata of file {} of repository {}",
                         event.getPath(), getRepository(event).getId(), e);
        }
    }

    private void downloadArtifactMetadataAtArtifactIdLevelFromRemote(RepositoryPath metadataRelativePath)
            throws Exception
    {
        proxyRepositoryArtifactResolver.fetchRemoteResource(metadataRelativePath);
    }

    private void downloadArtifactMetadataAtArtifactIdLevelFromRemoteAndMergeWithLocal(final RepositoryPath artifactAbsolutePath,
                                                                                      final RepositoryPath metadataPath)
            throws Exception
    {
        Repository repository = metadataPath.getRepository();
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        RestArtifactResolver client = restArtifactResolverFactory.newInstance(remoteRepository);
        
        Lock lock = repositoryPathLock.lock(metadataPath).writeLock();
        lock.lock();

        try (InputStream is = new BufferedInputStream(new ProxyRepositoryInputStream(client, metadataPath)))
        {
            mergeMetadata(artifactAbsolutePath, is);
        } 
        finally
        {
            lock.unlock();
        }
        
    }

    private void mergeMetadata(RepositoryPath repositoryPath,
                               InputStream remoteMetadataIs)
        throws IOException,
        NoSuchAlgorithmException,
        XmlPullParserException,
        ProviderImplementationException
    {
        MavenArtifact localArtifact = MavenArtifactUtils.convertPathToArtifact(repositoryPath);
        Metadata metadata = artifactMetadataService.getMetadata(remoteMetadataIs);
        
        artifactMetadataService.mergeMetadata(localArtifact, metadata);

    }

}
