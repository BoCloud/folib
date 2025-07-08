package com.folib.repository;

import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.storage.repository.HuggingFaceRepositoryFeatures;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.RepositoryFactory;
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
