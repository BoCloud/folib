package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.repository.PubRepositoryFeatures;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author leipenghui
 */
@Component
public class PubRepositoryFactory
        implements RepositoryFactory {

    @Inject
    private PubRepositoryFeatures pubRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(PubLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(pubRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
