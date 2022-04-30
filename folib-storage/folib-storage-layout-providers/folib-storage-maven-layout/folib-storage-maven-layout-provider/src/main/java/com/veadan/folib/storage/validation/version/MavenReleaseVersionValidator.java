package com.veadan.folib.storage.validation.version;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.veadan.folib.storage.validation.artifact.version.VersionValidationException;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryPolicyEnum;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author stodorov
 */
@Component
public class MavenReleaseVersionValidator
        implements MavenVersionValidator
{

    private static final Logger logger = LoggerFactory.getLogger(MavenReleaseVersionValidator.class);

    public static final String ALIAS = "maven-release-version-validator";

    public static final String DESCRIPTION = "Maven release version validator";

    @Inject
    private ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;


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
    public boolean supports(Repository repository)
    {
        return MavenVersionValidator.super.supports(repository) &&
               RepositoryPolicyEnum.RELEASE.getPolicy().equals(repository.getPolicy());
    }

    /**
     * Matches versions:
     * 1
     * 1.0
     * 1.0-SNAPSHOT
     */
    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException
    {
        String version = coordinates.getVersion();
        if (isRelease(version) && !repository.acceptsReleases())
        {
            throw new VersionValidationException("Cannot deploy a release artifact to a repository with a SNAPSHOT policy!");
        }
        if (!isRelease(version) && repository.acceptsReleases() && !repository.acceptsSnapshots())
        {
            throw new VersionValidationException("Cannot deploy a SNAPSHOT artifact to a repository with a release policy!");
        }
    }

}
