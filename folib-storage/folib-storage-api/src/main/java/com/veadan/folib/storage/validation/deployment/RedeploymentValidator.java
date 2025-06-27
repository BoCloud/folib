package com.veadan.folib.storage.validation.deployment;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.layout.LayoutProvider;
import com.veadan.folib.providers.layout.LayoutProviderRegistry;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.veadan.folib.storage.validation.artifact.version.VersionValidationException;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

/**
 * @author veadan
 */
@Component("redeploymentValidator")
public class RedeploymentValidator
        implements ArtifactDeploymentValidator
{

    private static final Logger logger = LoggerFactory.getLogger(RedeploymentValidator.class);

    public static final String ALIAS = "redeployment-validator";

    public static final String DESCRIPTION = "Re-deployment validator";

    @Lazy
    @Inject
    private LayoutProviderRegistry layoutProviderRegistry;
    @Lazy
    @Inject
    private ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;
    @Lazy
    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register()
    {
        artifactCoordinatesValidatorRegistry.addProvider(ALIAS, this);

        logger.info("Registered artifact coordinates validator '{}' with alias '{}'.",
                    getClass().getCanonicalName(), ALIAS);
    }

    @Override
    public String getAlias()
    {
        return ALIAS;
    }

    @Override
    public String getDescription()
    {
        return DESCRIPTION;
    }

    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException,
                   IOException
    {
        LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repository.getLayout());

        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, coordinates);
        
        if (repository.isAcceptsReleases() &&
            (!repository.isAllowsDeployment() && RepositoryFiles.artifactExists(repositoryPath)))
        {
            throw new VersionValidationException("The " + repository.getStorage().getId() + ":" +
                                                 repository.toString() +
                                                 " repository does not allow artifact re-deployment! (" +
                                                 coordinates.buildPath() + ")");
        }
    }

}
