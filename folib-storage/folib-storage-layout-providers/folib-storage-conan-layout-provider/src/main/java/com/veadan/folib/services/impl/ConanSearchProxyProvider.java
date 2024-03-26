package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.component.StorageClientComponent;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ConanSearchProvider;
import com.veadan.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.util.Objects;

/**
 * @author leipenghui
 **/
@Slf4j
@Component
public class ConanSearchProxyProvider implements ConanSearchProvider {

    @Inject
    private ConanSearchProviderRegistry conanSearchProviderRegistry;

    @Inject
    private StorageClientComponent clientComponent;

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @PostConstruct
    @Override
    public void register() {
        conanSearchProviderRegistry.addProvider(ConanSearchRepositoryTypeEnum.CONAN_PROXY.getType(), this);
        log.info("Registered conan search provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), ConanSearchRepositoryTypeEnum.CONAN_PROXY.getType());
    }

    @Override
    public SearchResults search(Repository repository, String query) {
        SearchResults searchResults = null;
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = "/v2/conans/search?q=" + query;
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        ResponseResult responseResult = clientComponent.doGet(repository.getStorage().getId(), repository.getId(), targetUrl);
        if (Objects.isNull(responseResult)) {
            return null;
        }
        if (HttpStatus.OK.value() == responseResult.getHttpStatus() && ResponseDataTypeEnum.JSON.equals(responseResult.getDataType()) && StringUtils.isNotBlank(responseResult.getData())) {
            searchResults = JSONObject.parseObject(responseResult.getData(), SearchResults.class);
        }
        return searchResults;
    }

    @Override
    public JSONObject revisionsSearch(Repository repository,  String artifactPath, String url) {
        JSONObject data = null;
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = url;
        if (!suffixUrl.startsWith(GlobalConstants.SEPARATOR)) {
            suffixUrl = GlobalConstants.SEPARATOR + suffixUrl;
        }
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        ResponseResult responseResult = clientComponent.doGet(repository.getStorage().getId(), repository.getId(), targetUrl);
        if (Objects.isNull(responseResult)) {
            return null;
        }
        if (HttpStatus.OK.value() == responseResult.getHttpStatus() && ResponseDataTypeEnum.JSON.equals(responseResult.getDataType()) && StringUtils.isNotBlank(responseResult.getData())) {
            data = JSONObject.parseObject(responseResult.getData());
        }
        return data;
    }


}
