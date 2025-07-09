package com.folib.repository;

import com.folib.providers.RpmLayoutProvider;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
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

