package com.veadan.folib.storage.validation.version;

import com.veadan.folib.storage.validation.PypiArtifactCoordinatesValidator;

/**
 * @author sainalshah
 */
public interface PypiVersionValidator
        extends PypiArtifactCoordinatesValidator
{


    boolean isPreRelease(String version);

    boolean isPostRelease(String version);

    boolean isDevelopmentalRelease(String version);

    boolean isLocalVersionIdentifierRelease(String version);

    boolean isFinalRelease(String version);
}
