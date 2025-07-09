package com.folib.repository;

import com.folib.providers.PhpLayoutProvider;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
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
