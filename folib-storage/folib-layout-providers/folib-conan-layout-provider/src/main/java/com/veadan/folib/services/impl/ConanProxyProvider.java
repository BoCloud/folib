package com.veadan.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.veadan.folib.components.StorageClientComponent;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.SearchResults;
import com.veadan.folib.domain.client.ResponseResult;
import com.veadan.folib.enums.ConanSearchRepositoryTypeEnum;
import com.veadan.folib.enums.ResponseDataTypeEnum;
import com.veadan.folib.providers.io.RepositoryFiles;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.services.ConanProvider;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import java.nio.file.Files;
import java.util.Objects;

/**
 * @author veadan
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

    @Inject
    private RepositoryPathResolver repositoryPathResolver;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

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
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
            return null;
        }
        String prefixUrl = remoteRepository.getUrl();
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
    public JSONObject revisions(Repository repository, String artifactPath, String targetUrl) {
        RepositoryPath indexJsonRepositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        try {
            JSONObject indexData = null;
            if (Objects.isNull(indexJsonRepositoryPath) || !Files.exists(indexJsonRepositoryPath) || RepositoryFiles.hasRefreshContent(indexJsonRepositoryPath)) {
                if (RepositoryFiles.hasRefreshContent(indexJsonRepositoryPath)) {
                    log.info("Conan indexJsonRepositoryPath [{}] [{}] [{}] refresh content", indexJsonRepositoryPath.getStorageId(), indexJsonRepositoryPath.getRepositoryId(), artifactPath);
                }
                indexData = commonUrlJSONData(repository, targetUrl);
                if (Objects.isNull(indexData)) {
                    if (Files.exists(indexJsonRepositoryPath)) {
                        return JSONObject.parseObject(Files.readString(indexJsonRepositoryPath));
                    }
                    return null;
                }
                try {
                    Files.writeString(indexJsonRepositoryPath, indexData.toJSONString());
                } catch (Exception ex) {
                    log.error(ExceptionUtils.getStackTrace(ex));
                }
            } else {
                indexData = JSONObject.parseObject(Files.readString(indexJsonRepositoryPath));
            }
            return indexData;
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    @Override
    public JSONObject getLocalRevisions(Repository repository, String artifactPath, String targetUrl) {
        RepositoryPath indexJsonRepositoryPath = repositoryPathResolver.resolve(repository, artifactPath);
        try {
            if (Objects.nonNull(indexJsonRepositoryPath) && Files.exists(indexJsonRepositoryPath) && !RepositoryFiles.hasRefreshContent(indexJsonRepositoryPath)) {
                return JSONObject.parseObject(Files.readString(indexJsonRepositoryPath));
            }
        } catch (Exception ex) {
            log.error(ExceptionUtils.getStackTrace(ex));
        }
        return null;
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
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
            return null;
        }
        String prefixUrl = remoteRepository.getUrl();
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
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", repository.getStorage().getId(), repository.getId(), remoteRepository.getUrl());
            return null;
        }
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
