package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.component.StorageClientComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.services.ConanProvider;
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
public class ConanProxyProvider implements ConanProvider {

    @Inject
    private ConanProviderRegistry conanProviderRegistry;

    @Inject
    private StorageClientComponent clientComponent;

    @Inject
    private ConfigurationManager configurationManager;

    @PostConstruct
    @Override
    public void register() {
        conanProviderRegistry.addProvider(ConanSearchRepositoryTypeEnum.CONAN_PROXY.getType(), this);
        log.info("Registered conan provider '[{}]' with alias '[{}]'.",
                getClass().getCanonicalName(), ConanSearchRepositoryTypeEnum.CONAN_PROXY.getType());
    }

    @Override
    public SearchResults search(String version, Repository repository, String query) {
        if (StringUtils.isBlank(query)) {
            query = "";
        }
        SearchResults searchResults = null;
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = String.format("/%s/conans/search?q=%s", version, query);
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
    public JSONObject revisionsSearch(Repository repository, String artifactPath, String url) {
        return commonUrlJSONData(repository, url);
    }

    @Override
    public JSONObject downloadUrls(Repository repository, String name, String version, String user, String channel) {
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = String.format("/v1/conans/%s/%s/%s/%s/download_urls", name, version, user, channel);
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        return commonJSONData(repository, targetUrl);
    }

    @Override
    public JSONObject packageDownloadUrls(Repository repository, String name, String version, String user, String channel, String packageId) {
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = String.format("/v1/conans/%s/%s/%s/%s/packages/%s/download_urls", name, version, user, channel, packageId);
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        return commonJSONData(repository, targetUrl);
    }

    @Override
    public JSONObject digest(Repository repository, String name, String version, String user, String channel) {
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = String.format("/v1/conans/%s/%s/%s/%s/digest", name, version, user, channel);
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        return commonJSONData(repository, targetUrl);
    }

    @Override
    public JSONObject packageDigest(Repository repository, String name, String version, String user, String channel, String packageId) {
        String prefixUrl = repository.getRemoteRepository().getUrl();
        String suffixUrl = String.format("/v1/conans/%s/%s/%s/%s/packages/%s/digest", name, version, user, channel, packageId);
        String targetUrl = StringUtils.removeEnd(prefixUrl, GlobalConstants.SEPARATOR) + suffixUrl;
        return commonJSONData(repository, targetUrl);
    }

    @Override
    public JSONObject getPackageInfo(Repository repository, String name, String version, String user, String channel, String packageId, String url) {
        return commonUrlJSONData(repository, url);
    }

    private JSONObject commonUrlJSONData(Repository repository, String url) {
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

    private JSONObject commonJSONData(Repository repository, String targetUrl) {
        JSONObject data = null;
        ResponseResult responseResult = clientComponent.doGet(repository.getStorage().getId(), repository.getId(), targetUrl);
        if (Objects.isNull(responseResult)) {
            return null;
        }
        if (HttpStatus.OK.value() == responseResult.getHttpStatus() && ResponseDataTypeEnum.JSON.equals(responseResult.getDataType()) && StringUtils.isNotBlank(responseResult.getData())) {
            String baseUrl = getRepositoryBaseUrl(repository);
            data = JSONObject.parseObject(responseResult.getData());
            String value = "";
            for (String key : data.keySet()) {
                value = data.getString(key);
                if (StringUtils.isNotBlank(value)) {
                    value = value.substring(value.indexOf("/v1/files/"));
                    data.put(key, baseUrl + value);
                }
            }
        }
        return data;
    }

    protected String getRepositoryBaseUrl(Repository repository) {
        return String.format("%s/storages/%s/%s", StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), "/"), repository.getStorage().getId(), repository.getId());
    }

}
