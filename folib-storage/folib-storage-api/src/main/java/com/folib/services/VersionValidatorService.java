package com.folib.services;

import com.folib.storage.validation.ArtifactCoordinatesValidator;

import java.util.Set;

/**
 * @author veadan
 */
public interface VersionValidatorService
{

    Set<ArtifactCoordinatesValidator> getVersionValidators();

}
