package com.folib.repository;

import com.folib.providers.layout.GoLayoutProvider;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@Component
public class GoRepositoryFactory
        implements RepositoryFactory {

    @Inject
    private GoRepositoryFeatures goRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(GoLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(goRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
