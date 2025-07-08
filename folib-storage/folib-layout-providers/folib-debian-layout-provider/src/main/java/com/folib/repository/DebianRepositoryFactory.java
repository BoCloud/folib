package com.folib.repository;

import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class DebianRepositoryFactory implements RepositoryFactory
{
    @Inject
    private DebianRepositoryFeatures debianRepositoryFeatures;

    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(DebianLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(new LinkedHashSet<>(debianRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}

