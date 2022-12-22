package com.veadan.folib.repository;

import com.veadan.folib.providers.layout.ConanLayoutProvider;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class ConanRepositoryFactory implements RepositoryFactory {

    @Inject
    private ConanRepositoryFeatures conanRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(ConanLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(conanRepositoryFeatures.getDefaultArtifactCoordinateValidators()));
        return repository;
    }
}
