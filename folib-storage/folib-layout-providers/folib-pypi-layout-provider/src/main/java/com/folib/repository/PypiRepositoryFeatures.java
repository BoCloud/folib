/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.repository;

import com.alibaba.fastjson.JSONObject;
import com.folib.artifact.coordinates.PypiCoordinates;
import com.folib.config.CustomAuthenticationFeature;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.constant.GlobalConstants;
import com.folib.domain.ArtifactIdGroup;
import com.folib.domain.ArtifactIdGroupEntity;
import com.folib.domain.PypiPackageInfo;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.folib.pypi.PypiSearchRequest;
import com.folib.pypi.PypiSearchResult;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import com.folib.util.CommonUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.*;
import java.util.concurrent.Executor;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * @author Veadan
 */
@Component
public class PypiRepositoryFeatures
        implements RepositoryFeatures {

    private static final Logger logger = LoggerFactory.getLogger(PypiRepositoryFeatures.class);
    private static final Pattern PACKAGE_NAME_PATTERN = Pattern.compile(PypiPackageInfo.NAME_FORMAT);
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    @Lazy
    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;
    @Lazy
    @Inject
    private PypiPackageFeedParser pypiPackageFeedParser;
    @Lazy
    @Inject
    private Executor eventTaskExecutor;
    @Lazy
    @Inject
    private RedeploymentValidator redeploymentValidator;

    @Lazy
    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;
    @Lazy
    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;

    private Set<String> defaultArtifactCoordinateValidators;

    @PostConstruct
    public void init() {
        defaultArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                genericReleaseVersionValidator.getAlias(),
                genericSnapshotVersionValidator.getAlias()));
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }


    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class PypiSearchPackagesEventListener {

        private PypiSearchRequest pypiSearchRequest;

        public void setPypiSearchRequest(PypiSearchRequest pypiSearchRequest) {
            this.pypiSearchRequest = pypiSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event) {
            if (pypiSearchRequest == null) {
                return;
            }

            String storageId = event.getStorageId();
            String repositoryId = event.getRepositoryId();

            Storage storage = getConfiguration().getStorage(storageId);
            Repository repository = storage.getRepository(repositoryId);
            RemoteRepository remoteRepository = repository.getRemoteRepository();
            if (remoteRepository == null) {
                return;
            }

            RepositorySearchRequest predicate = event.getPredicate();
            Boolean packageExists = packagesExists(storageId, repositoryId, predicate);

            logger.info("Pypi remote repository [{}] cached package existance is [{}]",
                    repository.getId(), packageExists);

            Runnable job = () -> fetchRemoteSearchResult(storageId, repositoryId, pypiSearchRequest);
            if (Boolean.FALSE.equals(packageExists)) {
                // Syncronously fetch remote package feed if ve have no cached
                // packages
                job.run();
            } else {
                eventTaskExecutor.execute(job);
            }

        }
    }

    private void fetchRemoteSearchResult(String storageId,
                                         String repositoryId,
                                         PypiSearchRequest pypiSearchRequest) {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return;
        }
        String targetUrl = "";
        if (remoteRepository.getUrl().endsWith("/")) {
            targetUrl = String.format("%s%s", remoteRepository.getUrl(), pypiSearchRequest.getPackageName());
        } else {
            targetUrl = String.format("%s/%s", remoteRepository.getUrl(), pypiSearchRequest.getPackageName());
        }
        Client restClient = null;
        Response response = null;
        List<PypiSearchResult> pypiSearchResult;
        try {
            restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            logger.debug("Search Pypi packages for [{}].", targetUrl);
            WebTarget service = restClient.target(targetUrl);
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            response = service.request(MediaType.TEXT_HTML).get();
            String responseBodyStr = response.readEntity(String.class);
            pypiSearchResult = extractSearchResult(repository, targetUrl, responseBodyStr);
            logger.info("Searched Pypi packages for [{}].", targetUrl);
        } catch (Exception e) {
            logger.error("Failed to search Pypi packages [{}]", targetUrl, e);
            return;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
            if (Objects.nonNull(restClient)) {
                restClient.close();
            }
        }
        try {
            pypiPackageFeedParser.parseSearchResult(repository, pypiSearchResult);
        } catch (Exception e) {
            logger.error("Failed to parse Pypi packages search result for [{}]", targetUrl, e);
        }
    }

    @Transactional
    public List<PypiSearchResult> fetchRemotePypiSearchResult(String storageId,
                                                              String repositoryId,
                                                              PypiSearchRequest pypiSearchRequest) {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return null;
        }
        String targetUrl = "";
        if (remoteRepository.getUrl().endsWith("/")) {
            targetUrl = String.format("%s%s", remoteRepository.getUrl(), pypiSearchRequest.getPackageName());
        } else {
            targetUrl = String.format("%s/%s", remoteRepository.getUrl(), pypiSearchRequest.getPackageName());
        }
        Client restClient = null;
        Response response = null;
        List<PypiSearchResult> pypiSearchResult;
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, pypiSearchRequest.getPackageName());
        try {
            restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            logger.debug("Search Pypi packages for [{}].", targetUrl);
            WebTarget service = restClient.target(targetUrl);
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            response = service.request(MediaType.TEXT_HTML).get();
            String responseBodyStr = response.readEntity(String.class);
            pypiSearchResult = extractSearchResult(repository, targetUrl, responseBodyStr);
            if (CollectionUtils.isNotEmpty(pypiSearchResult)) {
                Map<String, List<PypiSearchResult>> groupNameMap = pypiSearchResult.stream().collect(Collectors.groupingBy(PypiSearchResult::getGroupName));
                ArtifactIdGroup itemArtifactIdGroup = null;
                try {
                    long startTime = System.currentTimeMillis();
                    artifactIdGroup.setMetadata(JSONObject.toJSONString(pypiSearchResult));
                    artifactIdGroupRepository.saveOrUpdate(artifactIdGroup);
                    logger.debug("[{}] storage [{}] repository [{}] update artifactIdGroup [{}] metadata take time [{}] ms", this.getClass().getSimpleName(), storageId, repositoryId, artifactIdGroup.getUuid(), System.currentTimeMillis() - startTime);
                } catch (Exception ex) {
                    String realMessage = CommonUtils.getRealMessage(ex);
                    logger.warn("[{}] [{}] updateArtifactIdGroup error [{}]",
                            this.getClass().getSimpleName(), artifactIdGroup.getUuid(), realMessage);
                    if (CommonUtils.catchException(realMessage)) {
                        logger.warn("[{}] [{}] updateArtifactIdGroup catch error",
                                this.getClass().getSimpleName(), artifactIdGroup.getUuid());
                    }
                    throw new RuntimeException(ex);
                }
                for (Map.Entry<String, List<PypiSearchResult>> entry : groupNameMap.entrySet()) {
                    if (!entry.getKey().equals(pypiSearchRequest.getPackageName())) {
                        try {
                            itemArtifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, entry.getKey());
                            long startTime = System.currentTimeMillis();
                            itemArtifactIdGroup.setMetadata(JSONObject.toJSONString(entry.getValue()));
                            artifactIdGroupRepository.saveOrUpdate(itemArtifactIdGroup);
                            logger.debug("[{}] storage [{}] repository [{}] update itemArtifactIdGroup [{}] metadata take time [{}] ms", this.getClass().getSimpleName(), storageId, repositoryId, itemArtifactIdGroup.getUuid(), System.currentTimeMillis() - startTime);
                        } catch (Exception ex) {
                            String realMessage = CommonUtils.getRealMessage(ex);
                            logger.warn("[{}] [{}] itemArtifactIdGroup error [{}]",
                                    this.getClass().getSimpleName(), artifactIdGroup.getUuid(), realMessage);
                            if (CommonUtils.catchException(realMessage)) {
                                logger.warn("[{}] [{}] itemArtifactIdGroup catch error",
                                        this.getClass().getSimpleName(), artifactIdGroup.getUuid());
                            }
                            throw new RuntimeException(ex);
                        }
                    }
                }
            }
            logger.info("Searched Pypi packages for [{}].", targetUrl);
        } catch (Exception e) {
            logger.error("Failed to search Pypi packages [{}]", targetUrl, e);
            return null;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
            if (Objects.nonNull(restClient)) {
                restClient.close();
            }
        }
        if (CollectionUtils.isEmpty(pypiSearchResult)) {
            artifactIdGroup = artifactIdGroupRepository.findByArtifactIdGroup(artifactIdGroup.getUuid());
            if (Objects.nonNull(artifactIdGroup)) {
                String metadata = artifactIdGroup.getMetadata();
                if (StringUtils.isNotBlank(metadata)) {
                    pypiSearchResult = JSONObject.parseArray(metadata, PypiSearchResult.class);
                }
            }
        }
        return pypiSearchResult;
    }

    private Boolean packagesExists(String storageId,
                                   String repositoryId,
                                   RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonArtifactsExists(storageId, repositoryId,
                predicate.getArtifactId(),
                predicate.getCoordinateValues());
    }

    private List<PypiSearchResult> extractSearchResult(Repository repository, String targetUrl, String pypiSearchResult) {
        final String storageId = repository.getStorage().getId(), repositoryId = repository.getId();
        String prefix = "";
        if (targetUrl.contains("/storages/")) {
            prefix = targetUrl.substring(targetUrl.indexOf("/storages/"), targetUrl.indexOf("/simple/"));
            if (!prefix.endsWith(GlobalConstants.SEPARATOR)) {
                prefix = prefix + GlobalConstants.SEPARATOR;
            }
        }
        String finalPrefix = prefix;
        Matcher matcher = PACKAGE_NAME_PATTERN.matcher(pypiSearchResult);
        return matcher.results()
                .map(matchResult -> {
                    String artifactName = matchResult.group(2);
                    String artifactUrl = matchResult.group(1);
                    if (StringUtils.isNotBlank(finalPrefix) && artifactUrl.contains(finalPrefix)) {
                        artifactUrl = artifactUrl.replace(finalPrefix, "/../../");
                    }
                    artifactUrl = resolveUrl(targetUrl, artifactUrl);
                    return PypiSearchResult.builder().artifactName(artifactName).artifactUrl(artifactUrl).storageId(storageId).repositoryId(repositoryId).groupName(PypiCoordinates.parse(artifactName).getId()).build();
                })
                .collect(Collectors.toList());
    }

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

    /**
     * Client WebTarget 构建认证信息
     *
     * @param webTarget webTarget
     * @param username  username
     * @param password  password
     */
    public void authentication(WebTarget webTarget, String username, String password) {
        final CustomAuthenticationFeature customAuthenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? CustomAuthenticationFeature.create(username, password) : null;
        if (customAuthenticationFeature != null) {
            webTarget.register(customAuthenticationFeature);
            webTarget.property(ApacheClientProperties.REQUEST_CONFIG,
                    RequestConfig.custom().setCircularRedirectsAllowed(true).build());
        }
    }

    private static String resolveUrl(String baseUrl, String href) {
        if (href.startsWith("http://") || href.startsWith("https://")) {
            // Absolute URL, no need to resolve
            return href;
        }
        String resolvedUrl = baseUrl + href;
        while (resolvedUrl.contains("../")) {
            int index = resolvedUrl.indexOf("../");
            int slashIndex = resolvedUrl.lastIndexOf('/', index - 2);
            if (slashIndex != -1) {
                resolvedUrl = resolvedUrl.substring(0, slashIndex + 1) + resolvedUrl.substring(index + 3);
            } else {
                // Invalid URL, cannot resolve further
                break;
            }
        }
        return resolvedUrl;
    }
}
