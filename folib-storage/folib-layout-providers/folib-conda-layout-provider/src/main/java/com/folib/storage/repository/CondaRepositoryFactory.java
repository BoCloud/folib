package com.folib.storage.repository;

import com.folib.providers.layout.CondaLayoutProvider;
import com.folib.repository.CondaRepositoryFeatures;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;


@Component
public class CondaRepositoryFactory implements RepositoryFactory {
    @Inject
    private CondaRepositoryFeatures condaRepositoryFeatures;

    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(CondaLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(condaRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }
}
