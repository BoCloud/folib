package com.folib.event.artifact;

import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.repository.group.metadata.MavenMetadataGroupRepositoryComponent;
import com.folib.services.ArtifactMetadataService;
import com.folib.storage.metadata.MavenMetadataManager;
import com.folib.storage.repository.Repository;

import jakarta.inject.Inject;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public abstract class BaseMavenArtifactEventListener
{

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Inject
    ConfigurationManager configurationManager;

    @Inject
    ArtifactMetadataService artifactMetadataService;

    @Inject
    LayoutProviderRegistry layoutProviderRegistry;

    @Inject
    MavenMetadataManager mavenMetadataManager;

    @Inject
    MavenMetadataGroupRepositoryComponent mavenMetadataGroupRepositoryComponent;

    Repository getRepository(final ArtifactEvent<RepositoryPath> event)
    {
        return event.getPath().getFileSystem().getRepository();
    }

    void updateMetadataInGroupsContainingRepository(final ArtifactEvent<RepositoryPath> event,
                                                    final Function<RepositoryPath, RepositoryPath> artifactBasePathCalculation)
    {

        RepositoryPath artifactBasePath = artifactBasePathCalculation.apply(event.getPath());
        try
        {
            mavenMetadataGroupRepositoryComponent.updateGroupsContaining(artifactBasePath);
        }
        catch (Exception e)
        {
            logger.error("Unable to update parent group repositories metadata of file {}", event.getPath(), e);
        }
    }

}
