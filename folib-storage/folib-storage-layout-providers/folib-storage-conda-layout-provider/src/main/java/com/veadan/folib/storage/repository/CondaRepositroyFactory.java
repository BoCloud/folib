package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.CondaLayoutProvider;
import com.veadan.folib.repository.CondaRepositoryFeatures;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author LingengMa
 * @date 2025/04/02 14:08
 * @Description:
 */
@Component
public class CondaRepositroyFactory implements RepositoryFactory {
    @Inject
    private CondaRepositoryFeatures condaRepositoryFeatures;

    @Override
    public RepositoryDto createRepository(String repositoryId) {
        RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(CondaLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(
                new LinkedHashSet<>(condaRepositoryFeatures.getDefaultArtifactCoordinateValidators()));

        return repository;
    }
}
