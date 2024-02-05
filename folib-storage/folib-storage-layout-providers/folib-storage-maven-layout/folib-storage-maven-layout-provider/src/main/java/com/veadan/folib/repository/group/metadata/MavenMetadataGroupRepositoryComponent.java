package com.veadan.folib.repository.group.metadata;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.repository.group.BaseMavenGroupRepositoryComponent;
import com.veadan.folib.storage.metadata.MavenMetadataManager;
import com.veadan.folib.storage.repository.Repository;

import javax.inject.Inject;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;

import org.apache.maven.artifact.repository.metadata.Metadata;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class MavenMetadataGroupRepositoryComponent
        extends BaseMavenGroupRepositoryComponent
{

    @Inject
    private MavenMetadataManager mavenMetadataManager;

    @Override
    protected void cleanupGroupWhenArtifactPathNoLongerExistsInSubTree(final Repository groupRepository,
                                                                       final String artifactPath)
            throws IOException
    {
        final LayoutProvider layoutProvider = getRepositoryProvider(groupRepository);
        
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(groupRepository, artifactPath);
        Files.deleteIfExists(repositoryPath);
    }

    @Override
    protected UpdateCallback newInstance(RepositoryPath repositoryPath)
    {
        return new MetadataUpdateCallback(repositoryPath);
    }

    class MetadataUpdateCallback
            implements UpdateCallback
    {

        private final RepositoryPath initiatorRepositoryPath;

        private Metadata mergeMetadata;

        MetadataUpdateCallback(RepositoryPath repositoryPath)
        {
            this.initiatorRepositoryPath = repositoryPath;
        }

        @Override
        public void beforeUpdate()
                throws IOException
        {
            final RepositoryPath artifactAbsolutePath = initiatorRepositoryPath.toAbsolutePath();

            try
            {
                mergeMetadata = mavenMetadataManager.readMetadata(artifactAbsolutePath);
            }
            catch (final FileNotFoundException ex)
            {
                logger.warn("Unable to read metadata in repository path {}.", artifactAbsolutePath);
                throw new StopUpdateSilentlyException();
            }
            catch (final XmlPullParserException e)
            {
                throw new IOException(e);
            }
        }

        @Override
        public void performUpdate(final RepositoryPath parentRepositoryArtifactAbsolutePath)
                throws IOException
        {
            mavenMetadataManager.mergeAndStore(parentRepositoryArtifactAbsolutePath, mergeMetadata);
        }
    }

}
