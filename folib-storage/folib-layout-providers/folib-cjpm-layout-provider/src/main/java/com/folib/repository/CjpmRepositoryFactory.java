package com.folib.repository;

import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
import com.folib.providers.layout.CjpmLayoutProvider;
import org.springframework.stereotype.Component;
import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class CjpmRepositoryFactory
        implements RepositoryFactory
{

    @Inject
    private CjpmRepositoryFeatures cjpmRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(CjpmLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(cjpmRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
