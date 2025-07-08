package com.folib.repository;

import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@Component
public class GoRepositoryFeatures
        implements RepositoryFeatures
{

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();


    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultArtifactCoordinateValidators;
    }

}
