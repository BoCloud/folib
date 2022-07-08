package com.veadan.folib.repository;

import java.util.LinkedHashSet;
import java.util.Set;

import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class RawRepositoryFeatures
        implements RepositoryFeatures
{

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();


    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultArtifactCoordinateValidators;
    }

}
