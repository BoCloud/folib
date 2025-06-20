package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.repository.RawRepositoryFeatures;

import javax.inject.Inject;
import java.util.LinkedHashSet;

import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class RawRepositoryFactory
        implements RepositoryFactory
{

    @Inject
    private RawRepositoryFeatures rawRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(RawLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(rawRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
