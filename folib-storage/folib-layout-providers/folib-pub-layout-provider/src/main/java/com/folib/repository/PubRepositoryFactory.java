package com.folib.repository;

import com.folib.providers.layout.PubLayoutProvider;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;
import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author veadan
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
