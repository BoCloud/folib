package com.veadan.folib.repository;

import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author veadan
 * @date 2023/8/3 13:59
 */
@Component
public class CocoapodsRepositoryFeatures implements RepositoryFeatures
{
    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();
    
    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() 
    {
        return defaultArtifactCoordinateValidators;
    }
}
