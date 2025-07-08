package com.folib.repository;

import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.Set;

@Component
public class HelmRepositoryFeatures implements RepositoryFeatures{
    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();


    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultArtifactCoordinateValidators;
    }
}
