package com.folib.storage.validation.artifactid;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.folib.providers.io.RepositoryFileAttributes;
import com.folib.providers.io.RepositoryPath;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.storage.validation.artifact.LowercaseValidationException;
import com.folib.storage.repository.Repository;
import com.folib.storage.validation.MavenArtifactCoordinatesValidator;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.IOException;
import java.nio.file.Files;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * Created by dinesh on 12/10/17.
 */
@Component
public class MavenArtifactIdLowercaseValidator
        implements MavenArtifactCoordinatesValidator
{


    private static final Logger logger = LoggerFactory.getLogger(MavenArtifactIdLowercaseValidator.class);

    public static final String ALIAS = "maven-artifactid-lowercase-validator";

    public static final String DESCRIPTION = "Maven artifactId lowercase validator";

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
        if (!mac.getArtifactId().toLowerCase().equals(mac.getArtifactId()))
        {
            throw new LowercaseValidationException("The artifactId should be defined in lowercase.");
        }
    }

    public RepositoryFileAttributes getAttributes(RepositoryPath repositoryPath)
            throws IOException
    {
        return Files.readAttributes(repositoryPath, RepositoryFileAttributes.class);
    }

}
