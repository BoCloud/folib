package com.folib.storage.repository;

import com.folib.repository.RepositoryFeatures;
import org.springframework.stereotype.Component;

import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author Veadan
 */
@Component
public class GitLfsRepositoryFeatures
        implements RepositoryFeatures
{

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();


    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultArtifactCoordinateValidators;
    }

}
