package com.folib.storage.repository;

import com.folib.providers.layout.PhpLayoutProvider;
import com.folib.repository.PhpRepositoryFeatures;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author veadan
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
