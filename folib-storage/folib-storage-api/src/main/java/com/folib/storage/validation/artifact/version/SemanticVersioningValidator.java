package com.folib.storage.validation.artifact.version;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.versioning.SemanticVersion;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.ArtifactCoordinatesValidator;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class SemanticVersioningValidator
        implements ArtifactCoordinatesValidator
{

    private static final Logger logger = LoggerFactory.getLogger(SemanticVersioningValidator.class);

    public static final String ALIAS = "semantic-version-validator";

    public static final String DESCRIPTION = "Semantic version validator";

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

        try
        {
            SemanticVersion.parse(version);
        }
        catch (IllegalArgumentException e)
        {
            throw new VersionValidationException(String.format("Artifact version [%s] should follow the Semantic " +
                                                               "Versioning specification (https://semver.org/).",
                                                               version));
        }
    }

}
