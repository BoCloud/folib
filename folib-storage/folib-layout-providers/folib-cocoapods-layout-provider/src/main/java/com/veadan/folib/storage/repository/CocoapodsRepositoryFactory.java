package com.veadan.folib.storage.repository;

import com.veadan.folib.providers.layout.CocoapodsLayoutProvider;
import com.veadan.folib.repository.CocoapodsRepositoryFeatures;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.RepositoryFactory;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.LinkedHashSet;

/**
 * @author veadan
 * @date 2023/8/3 13:58
 */
@Component
public class CocoapodsRepositoryFactory implements RepositoryFactory 
{
    @Inject
    private CocoapodsRepositoryFeatures cocoapodsRepositoryFeatures;
    
    @Override
    public RepositoryDto createRepository(String repositoryId) 
    {
        final RepositoryDto repository = new RepositoryDto(repositoryId);
        repository.setLayout(CocoapodsLayoutProvider.ALIAS);
        repository.setArtifactCoordinateValidators(new LinkedHashSet<>(cocoapodsRepositoryFeatures.getDefaultArtifactCoordinateValidators()));
        return repository;
    }
}
