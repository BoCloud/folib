package com.veadan.folib.repository;

import java.util.Set;

/**
 * @author Veadan
 */
public interface RepositoryFeatures
{

    /**
     * Returns the default list of artifact coordinate validators.
     *
     * @return
     */
    Set<String> getDefaultArtifactCoordinateValidators();

}
