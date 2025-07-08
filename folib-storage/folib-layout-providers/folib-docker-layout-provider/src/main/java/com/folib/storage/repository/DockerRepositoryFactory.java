package com.folib.storage.repository;

import com.folib.providers.layout.DockerLayoutProvider;
import com.folib.repository.DockerRepositoryFeatures;

import javax.inject.Inject;
import java.util.LinkedHashSet;

import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component
public class DockerRepositoryFactory
        implements RepositoryFactory
{

    @Inject
    private DockerRepositoryFeatures dockerRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(DockerLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(new LinkedHashSet<>(dockerRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
