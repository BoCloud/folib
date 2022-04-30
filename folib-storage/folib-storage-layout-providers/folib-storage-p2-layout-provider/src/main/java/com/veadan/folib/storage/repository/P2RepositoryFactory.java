package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.P2LayoutProvider;
import com.veadan.folib.repository.P2RepositoryFeatures;

import javax.inject.Inject;
import java.util.LinkedHashSet;

import org.springframework.stereotype.Component;

/**
 * @author carlspring
 */
@Component
public class P2RepositoryFactory
        implements RepositoryFactory
{

    @Inject
    private P2RepositoryFeatures p2RepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(P2LayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(p2RepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
