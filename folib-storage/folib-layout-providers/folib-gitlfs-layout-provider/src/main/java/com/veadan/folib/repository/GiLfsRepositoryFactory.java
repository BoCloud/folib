package com.veadan.folib.repository;


import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.storage.repository.GitLfsRepositoryFeatures;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class GiLfsRepositoryFactory implements RepositoryFactory
{

    @Inject
    private GitLfsRepositoryFeatures gitLfsRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(GitLfsLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(gitLfsRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
