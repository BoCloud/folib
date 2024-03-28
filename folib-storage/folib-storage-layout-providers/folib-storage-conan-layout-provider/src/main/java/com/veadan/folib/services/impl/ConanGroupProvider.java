package com.veadan.folib.services.impl;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.configuration.ConfigurationUtils;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.services.ConanSearchProvider;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.compress.utils.Lists;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class ConanSearchGroupProvider implements ConanSearchProvider {

    @Inject
    private ConanSearchProviderRegistry conanSearchProviderRegistry;

    @Inject
    protected ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        conanSearchProviderRegistry.addProvider(ConanSearchRepositoryTypeEnum.CONAN_GROUP.getType(), this);
        log.info("Registered conan search provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), ConanSearchRepositoryTypeEnum.CONAN_GROUP.getType());
    }

    @Override
    public SearchResults search(Repository repository, String query) {
        SearchResults searchResults = SearchResults.builder().results(Lists.newArrayList()).build(), subSearchResults;
        for (String storageAndRepositoryId : repository.getGroupRepositories()) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManager.getRepository(sId, rId);
            if (!isRepositoryResolvable(subRepository)) {
                continue;
            }
            ConanSearchProvider conanSearchProvider = conanSearchProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
            subSearchResults = conanSearchProvider.search(subRepository, query);
            if (Objects.nonNull(subSearchResults) && CollectionUtils.isNotEmpty(subSearchResults.getResults())) {
                searchResults.getResults().addAll(subSearchResults.getResults());
            }
        }
        return searchResults;
    }

    @Override
    public JSONObject revisionsSearch(Repository repository, String artifactPath, String url) {
        JSONObject data = new JSONObject(), subData;
        for (String storageAndRepositoryId : repository.getGroupRepositories()) {
            String sId = ConfigurationUtils.getStorageId(repository.getStorage().getId(), storageAndRepositoryId);
            String rId = ConfigurationUtils.getRepositoryId(storageAndRepositoryId);
            Repository subRepository = configurationManager.getRepository(sId, rId);
            if (!isRepositoryResolvable(subRepository)) {
                continue;
            }
            ConanSearchProvider conanSearchProvider = conanSearchProviderRegistry.getProvider(ConanSearchRepositoryTypeEnum.resolveType(subRepository.getType()));
            subData = conanSearchProvider.revisionsSearch(subRepository, artifactPath, url);
            if (Objects.nonNull(subData) && !JSONUtil.isNull(subData) && subData.keySet().size() > 0) {
                data = subData;
                break;
            }
        }
        return data;
    }

    public boolean isRepositoryResolvable(Repository repository) {
        final boolean isInService = repository.isInService();
        if (!isInService) {
            log.info("- Repository [{}] is not in service, skipping...",
                    repository.getStorageIdAndRepositoryId());
            return false;
        }
        return true;
    }

}
