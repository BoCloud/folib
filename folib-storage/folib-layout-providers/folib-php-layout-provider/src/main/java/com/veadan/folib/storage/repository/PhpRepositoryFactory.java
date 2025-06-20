package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.PhpLayoutProvider;
import com.veadan.folib.repository.PhpRepositoryFeatures;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author leipenghui
 */
@Component
public class PhpRepositoryFactory
        implements RepositoryFactory {

    @Inject
    private PhpRepositoryFeatures phpRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(PhpLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(phpRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
