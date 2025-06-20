package com.veadan.folib.repository;

import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.Set;

@Component
public class ConanRepositoryFeatures implements RepositoryFeatures{
    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();


    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultArtifactCoordinateValidators;
    }
}
