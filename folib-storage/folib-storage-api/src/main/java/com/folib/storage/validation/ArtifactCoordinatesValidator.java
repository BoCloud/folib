package com.folib.storage.validation;

import com.folib.artifact.coordinates.ArtifactCoordinates;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidationException;
import com.folib.storage.validation.artifact.version.VersionValidationException;
import com.folib.providers.ProviderImplementationException;
import com.folib.storage.repository.Repository;

import java.io.IOException;
import java.util.Collections;
import java.util.Set;

/**
 * @author veadan
 */
public interface ArtifactCoordinatesValidator
{

    void register();

    String getAlias();

    String getDescription();

    default boolean supports(Repository repository)
    {
        return true;
    }

    default boolean supports(String layoutProvider)
    {
        return true;
    }

    /**
     * Checks if an artifact version is acceptable by the repository.
     *
     * @param repository  The repository.
     * @param coordinates The artifact being deployed.
     */
    void validate(Repository repository,
                  ArtifactCoordinates coordinates)
            throws VersionValidationException,
                   ProviderImplementationException,
            ArtifactCoordinatesValidationException,
                   IOException;

    /**
     * Returns the list of supported validators. By default, it returns an empty set, meaning that there is
     * no explicit list of supported providers to be limited to, (hence it accepts any provider).
     *
     * @return
     */
    default Set<String> getSupportedLayoutProviders()
    {
        return Collections.emptySet();
    }

}
