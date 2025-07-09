package com.folib.repository;

import com.folib.providers.HelmLayoutProvider;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class HelmRepositoryFactory implements RepositoryFactory {

    @Inject
    private HelmRepositoryFeatures helmRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(HelmLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(helmRepositoryFeatures.getDefaultArtifactCoordinateValidators()));
        return repository;
    }
}