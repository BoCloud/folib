package com.veadan.folib.repository;

import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.storage.repository.HuggingFaceRepositoryFeatures;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;


@Component
public class HuggingFaceRepositoryFactory implements RepositoryFactory {

    @Inject
    private HuggingFaceRepositoryFeatures huggingFaceRepositoryFeatures;


    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(HuggingFaceLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(huggingFaceRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }

}
