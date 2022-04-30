package com.veadan.folib.services;

import com.veadan.folib.storage.validation.ArtifactCoordinatesValidator;

import java.util.Set;

/**
 * @author mtodorov
 */
public interface VersionValidatorService
{

    Set<ArtifactCoordinatesValidator> getVersionValidators();

}
