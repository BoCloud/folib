package com.veadan.folib.storage.validation.artifact.version;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.ArtifactCoordinatesValidator;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 * @author veadan
 */
@Component
public class GenericSnapshotVersionValidator
        implements ArtifactCoordinatesValidator
{

    private static final Logger logger = LoggerFactory.getLogger(GenericSnapshotVersionValidator.class);

    public static final String ALIAS = "generic-snapshot-version-validator";

    public static final String DESCRIPTION = "Generic release version validator";

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
        return repository.getArtifactCoordinateValidators().contains(ALIAS);
    }

    @Override
    public boolean supports(String layoutProvider)
    {
        return true;
    }

    @Override
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws VersionValidationException
    {
        String version = coordinates.getVersion();
        if (isSnapshot(version) && !repository.isAcceptsSnapshots())
        {
            throw new VersionValidationException("Cannot deploy a SNAPSHOT artifact to a repository which " +
                                                 "doesn't accept SNAPSHOT policy!");
        }
        if (!isSnapshot(version) && repository.isAcceptsSnapshots() && !repository.isAcceptsReleases())
        {
            throw new VersionValidationException("Cannot deploy a release artifact to a repository with " +
                                                 "a SNAPSHOT policy!");
        }
    }

    public boolean isSnapshot(String version)
    {
        return StringUtils.isNotBlank(version) && StringUtils.endsWithIgnoreCase(version, "-SNAPSHOT");
    }

}

