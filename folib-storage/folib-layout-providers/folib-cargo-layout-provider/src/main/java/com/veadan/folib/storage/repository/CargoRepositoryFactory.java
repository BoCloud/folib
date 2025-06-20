package com.veadan.folib.storage.repository;

import com.veadan.folib.layout.providers.CargoLayoutProvider;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

@Component
public class CargoRepositoryFactory  implements RepositoryFactory
{

    @Inject
    private CargoRepositoryFeatures cargoRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId)
    {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(CargoLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(cargoRepositoryFeatures.getDefaultArtifactCoordinateValidators()));
        return repository;
    }
}
