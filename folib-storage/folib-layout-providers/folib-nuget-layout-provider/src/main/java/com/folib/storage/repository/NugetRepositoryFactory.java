package com.folib.storage.repository;

import com.folib.providers.NugetLayoutProvider;
import com.folib.repository.NugetRepositoryFeatures;

import javax.inject.Inject;
import java.util.LinkedHashSet;

import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class NugetRepositoryFactory
        implements RepositoryFactory
{

    @Inject
    private NugetRepositoryFeatures nugetRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(NugetLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(nugetRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
