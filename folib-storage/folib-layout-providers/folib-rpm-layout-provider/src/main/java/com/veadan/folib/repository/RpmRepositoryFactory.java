package com.veadan.folib.repository;

import com.veadan.folib.providers.layout.RpmLayoutProvider;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class RpmRepositoryFactory implements RepositoryFactory
{

    @Inject
    private RpmRepositoryFeatures rpmRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(RpmLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(new LinkedHashSet<>(rpmRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}

