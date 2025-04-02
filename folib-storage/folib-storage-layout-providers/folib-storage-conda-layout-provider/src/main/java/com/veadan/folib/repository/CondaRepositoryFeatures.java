package com.veadan.folib.repository;

import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author LingengMa
 * @date 2025-04-02 13:36
 */
@Component
public class CondaRepositoryFeatures implements RepositoryFeatures {

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }
}

