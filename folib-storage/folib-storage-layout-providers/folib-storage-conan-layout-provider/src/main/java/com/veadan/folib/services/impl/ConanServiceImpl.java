package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.services.ConanSearchProvider;
import com.veadan.folib.services.ConanService;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.inject.Inject;

/**
 * @author leipenghui
 * @date 2024/3/25
 **/
@Slf4j
@Service
public class ConanServiceImpl implements ConanService {

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ConanSearchProviderRegistry conanSearchProviderRegistry;

    @Override
    public SearchResults search(Repository repository, String query) {
        ConanSearchProvider conanSearchProvider = conanSearchProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.search(repository, query);
    }

    @Override
    public Object revisionsSearch(Repository repository, String artifactPath, String url) {
        ConanSearchProvider conanSearchProvider = conanSearchProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(repository.getType()));
        return conanSearchProvider.revisionsSearch(repository, artifactPath, url);
    }
}
