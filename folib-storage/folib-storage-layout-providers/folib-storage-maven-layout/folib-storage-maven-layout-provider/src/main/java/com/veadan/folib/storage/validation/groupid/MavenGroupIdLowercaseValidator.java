package com.veadan.folib.storage.validation.groupid;

import com.veadan.folib.artifact.coordinates.ArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.providers.io.RepositoryFileAttributes;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.veadan.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.veadan.folib.storage.validation.artifact.LowercaseValidationException;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.validation.MavenArtifactCoordinatesValidator;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Created by dinesh on 12/6/17.
 */
@Component
public class MavenGroupIdLowercaseValidator
        implements MavenArtifactCoordinatesValidator
{

    private static final Logger logger = LoggerFactory.getLogger(MavenGroupIdLowercaseValidator.class);

    public static final String ALIAS = "maven-groupid-lowercase-validator";

    public static final String DESCRIPTION = "Maven groupId lowercase validator";

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
    public void validate(Repository repository,
                         ArtifactCoordinates coordinates)
            throws ArtifactCoordinatesValidationException
    {
        MavenArtifactCoordinates mac = (MavenArtifactCoordinates) coordinates;
        if (!mac.getGroupId().toLowerCase().equals(mac.getGroupId()))
        {
            throw new LowercaseValidationException("The groupId should be defined in lowercase.");
        }
    }

    public RepositoryFileAttributes getAttributes(RepositoryPath repositoryPath)
            throws IOException
    {
        return Files.readAttributes(repositoryPath, RepositoryFileAttributes.class);
    }

}

