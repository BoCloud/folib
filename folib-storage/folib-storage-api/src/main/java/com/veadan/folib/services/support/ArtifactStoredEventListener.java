package com.veadan.folib.services.support;

import java.io.IOException;
import java.util.Set;
import java.util.stream.Collectors;

import javax.inject.Inject;

import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.artifact.AsyncArtifactEntryHandler;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.ArtifactArchiveListing;
import com.veadan.folib.event.artifact.ArtifactEventTypeEnum;
import com.veadan.folib.storage.repository.Repository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component
public class ArtifactStoredEventListener
        extends AsyncArtifactEntryHandler
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactStoredEventListener.class);

    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;

    public ArtifactStoredEventListener()
    {
        super(ArtifactEventTypeEnum.EVENT_ARTIFACT_FILE_STORED);
    }

    @Override
    protected Artifact handleEvent(RepositoryPath repositoryPath)
            throws IOException
    {
        Artifact artifactEntry = repositoryPath.getArtifactEntry();

        if (artifactEntry == null)
        {
            logger.warn("No [{}] for [{}].",
                        Artifact.class.getSimpleName(),
                        repositoryPath);

            return null;
        }

        Repository repository = repositoryPath.getRepository();
        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());
        Set<String> archiveFilenames = layoutProvider.listArchiveFilenames(repositoryPath);
        if (archiveFilenames.isEmpty())
        {
            return null;
        }
        else if (archiveFilenames.size() > 5)
        {
            //TODO: issues/1752 
            archiveFilenames = archiveFilenames.stream().limit(100).collect(Collectors.toSet());
        }

        ArtifactArchiveListing artifactArchiveListing = artifactEntry.getArtifactArchiveListing();
        artifactArchiveListing.setFilenames(archiveFilenames);

        return artifactEntry;
    }

}
