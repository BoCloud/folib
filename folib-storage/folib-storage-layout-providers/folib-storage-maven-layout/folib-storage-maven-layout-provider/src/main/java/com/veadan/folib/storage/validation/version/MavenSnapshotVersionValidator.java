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
public class MavenSnapshotVersionValidator
        implements MavenVersionValidator
{

    private static final Logger logger = LoggerFactory.getLogger(MavenSnapshotVersionValidator.class);

    public static final String ALIAS = "maven-snapshot-version-validator";

    public static final String DESCRIPTION = "Maven snapshot version validator";

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
               RepositoryPolicyEnum.SNAPSHOT.getPolicy().equals(repository.getPolicy());
    }

    /**
     * Matches versions:
     * 1.0-20131004
     * 1.0-20131004.115330
     * 1.0-20131004.115330-1
     * 1.0.8-20151025.032208-1
     * 1.0.8-alpha-1-20151025.032208-1
     */
    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException
    {
        String version = coordinates.getVersion();
        if (isSnapshot(version) && !repository.isAcceptsSnapshots())
        {
            throw new VersionValidationException("Cannot deploy a SNAPSHOT artifact to a repository with a release policy!");
        }
        if (!isSnapshot(version) && repository.isAcceptsSnapshots() && !repository.isAcceptsReleases())
        {
            throw new VersionValidationException("Cannot deploy a release artifact to a repository with a SNAPSHOT policy!");
        }
    }

}
