package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.PypiLayoutProvider;
import com.veadan.folib.repository.PypiRepositoryFeatures;

import javax.inject.Inject;
import java.util.LinkedHashSet;

import org.springframework.stereotype.Component;

/**
 * @author carlspring
 */
@Component
public class PypiRepositoryFactory
        implements RepositoryFactory
{

    @Inject
    private PypiRepositoryFeatures pypiRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(PypiLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(new LinkedHashSet<>(pypiRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
