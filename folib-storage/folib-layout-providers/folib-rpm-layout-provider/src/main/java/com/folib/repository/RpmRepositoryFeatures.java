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

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.config.CustomAuthenticationFeature;
import com.folib.config.RpmLayoutProviderConfig;
import com.folib.configuration.Configuration;
import com.folib.configuration.ConfigurationManager;
import com.folib.npm.metadata.Change;
import com.folib.npm.metadata.PackageFeed;
import com.folib.npm.metadata.SearchResults;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.rpm.RpmSearchRequest;
import com.folib.rpm.RpmViewRequest;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.Storage;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryData;
import com.folib.storage.repository.RepositoryDto;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import com.folib.configuration.RpmRepositoryConfigurationData;
import com.folib.configuration.remote.RpmRemoteRepositoryConfiguration;
import com.folib.configuration.remote.RpmRemoteRepositoryConfigurationDto;
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
import org.springframework.util.Assert;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.concurrent.Executor;
@Component
public class RpmRepositoryFeatures implements RepositoryFeatures
{
    private static final Logger logger = LoggerFactory.getLogger(RpmRepositoryFeatures.class);

    private static final int CHANGES_BATCH_SIZE = 500;

    private static final boolean ALLOWS_UNPUBLISH_DEFAULT = true;

    @Lazy
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Lazy
    @Inject
    private RedeploymentValidator redeploymentValidator;
    @Lazy
    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;
    @Lazy
    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;
    @Lazy
    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;
    @Lazy
    @Inject
    private ConfigurationManager configurationManager;
    @Lazy
    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    @Lazy
    @Inject
    private Executor eventTaskExecutor;
    @Lazy
    @Inject
    @RpmLayoutProviderConfig.RpmObjectMapper
    private ObjectMapper rpmJacksonMapper;
    @Lazy
    @Inject
    private RpmPackageFeedParser rpmPackageFeedParser;

    private Set<String> defaultArtifactCoordinateValidators;

    @PostConstruct
    public void init()
    {
        defaultArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                genericReleaseVersionValidator.getAlias(),
                genericSnapshotVersionValidator.getAlias()));
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultArtifactCoordinateValidators;
    }

    public boolean allowsUnpublish(String storageId,
                                   String repositoryId)
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        Optional<RpmRepositoryConfigurationData> repositoryConfiguration = Optional.ofNullable(
                (RpmRepositoryConfigurationData) repository.getRepositoryConfiguration());
        boolean allowsUnpublish = repositoryConfiguration.map(RpmRepositoryConfigurationData::isAllowsUnpublish)
                .orElse(ALLOWS_UNPUBLISH_DEFAULT);

        logger.info("allowsUnpublish is [{}] for storageId: [{}]; repositoryId: [{}]",
                allowsUnpublish,
                storageId,
                repositoryId);

        return allowsUnpublish;
    }

    private void fetchRemoteSearchResult(String storageId,
                                         String repositoryId,
                                         String text,
                                         Integer size)
    {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null)
        {
            return;
        }
        String remoteRepositoryUrl = remoteRepository.getUrl();

        SearchResults searchResults;
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId,repositoryId);
        try
        {
            logger.info("Search NPM packages for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            service = service.path("-/v1/search").queryParam("text", text).queryParam("size", size);

            InputStream inputStream = service.request().buildGet().invoke(InputStream.class);
            searchResults = rpmJacksonMapper.readValue(inputStream, SearchResults.class);

            logger.info("Searched NPM packages for [{}].", remoteRepository.getUrl());

        }
        catch (Exception e)
        {
            logger.error("Failed to search NPM packages [{}]", remoteRepositoryUrl, e);

            return;
        }
        finally
        {
            restClient.close();
        }

        try
        {
            rpmPackageFeedParser.parseSearchResult(repository, searchResults);
        }
        catch (Exception e)
        {
            logger.error("Failed to parse NPM packages search result for [{}]", remoteRepositoryUrl, e);
        }
    }

    public void fetchRemoteChangesFeed(String storageId,
                                       String repositoryId)
            throws IOException
    {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null)
        {
            return;
        }

        RepositoryDto mutableRepository = configurationManagementService.getMutableConfigurationClone()
                .getStorage(storageId)
                .getRepository(repositoryId);
        RpmRemoteRepositoryConfigurationDto mutableConfiguration = (RpmRemoteRepositoryConfigurationDto) mutableRepository.getRemoteRepository()
                .getCustomConfiguration();

        RpmRemoteRepositoryConfiguration configuration = (RpmRemoteRepositoryConfiguration) remoteRepository.getCustomConfiguration();
        if (configuration == null)
        {
            logger.warn("Remote npm configuration not found for [{}]/[{}]", storageId, repositoryId);
            return;
        }
        Long lastCnahgeId = configuration.getLastChangeId();
        String replicateUrl = configuration.getReplicateUrl();

        Long nextChangeId = lastCnahgeId;
        do
        {
            lastCnahgeId = nextChangeId;
            mutableConfiguration.setLastChangeId(nextChangeId);
            configurationManagementService.saveRepository(storageId, mutableRepository);

            nextChangeId = Long.valueOf(fetchRemoteChangesFeed(repository, replicateUrl, lastCnahgeId + 1));
        } while (nextChangeId > lastCnahgeId);
    }

    private Integer fetchRemoteChangesFeed(Repository repository,
                                           String replicateUrl,
                                           Long since)
            throws IOException
    {
        int result = 0;
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(repository.getStorage().getId(),repository.getId());
        try
        {
            logger.info("Fetching remote changes for [{}] since [{}].", replicateUrl, since);

            WebTarget service = restClient.target(replicateUrl);
            authentication(service, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
            service = service.path("_changes");
            service = service.queryParam("since", since);
            service = service.queryParam("include_docs", true);
            service = service.queryParam("limit", CHANGES_BATCH_SIZE);

            Invocation request = service.request().buildGet();

            result = fetchRemoteChangesFeed(repository, request);
        }
        finally
        {
            restClient.close();
        }

        return result;
    }

    private int fetchRemoteChangesFeed(Repository repository,
                                       Invocation request)
            throws IOException
    {
        int result = 0;

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        RpmRemoteRepositoryConfiguration repositoryConfiguration = (RpmRemoteRepositoryConfiguration) remoteRepository.getCustomConfiguration();

        JsonFactory jfactory = new JsonFactory();

        try (InputStream is = request.invoke(InputStream.class))
        {

            JsonParser jp = jfactory.createParser(is);
            jp.setCodec(rpmJacksonMapper);

            Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT, "npm changes feed should be JSON object.");
            Assert.isTrue(jp.nextFieldName().equals("results"), "npm changes feed should contains `results` field.");
            Assert.isTrue(jp.nextToken() == JsonToken.START_ARRAY, "npm changes feed `results` should be array.");

            StringBuffer sb = new StringBuffer();
            while (jp.nextToken() != null)
            {
                JsonToken nextToken = jp.currentToken();
                if (nextToken == JsonToken.END_ARRAY)
                {
                    break;
                }

                JsonNode node = jp.readValueAsTree();
                sb.append(node.toString());

                String changeValue = sb.toString();

                Change change;
                try
                {
                    change = rpmJacksonMapper.readValue(changeValue, Change.class);
                }
                catch (Exception e)
                {
                    logger.error("Failed to parse NPM changes feed [{}] since [{}]: \n {}",
                            repositoryConfiguration.getReplicateUrl(),
                            repositoryConfiguration.getLastChangeId(),
                            changeValue,
                            e);

                    return result;
                }

                PackageFeed packageFeed = change.getDoc();
                try
                {
                    rpmPackageFeedParser.parseFeed(repository, packageFeed);
                }
                catch (Exception e)
                {
                    logger.error("Failed to parse NPM feed [{}/{}]",
                            ((RepositoryData)repository).getRemoteRepository().getUrl(),
                            packageFeed.getName(),
                            e);

                }

                result = change.getSeq();
                sb = new StringBuffer();
            }

        }

        logger.info("Fetched remote changes for  [{}] since [{}].",
                repositoryConfiguration.getReplicateUrl(),
                repositoryConfiguration.getLastChangeId());

        return result;
    }

    private void fetchRemotePackageFeed(String storageId,
                                        String repositoryId,
                                        String packageId)
    {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null)
        {
            return;
        }
        String remoteRepositoryUrl = remoteRepository.getUrl();

        PackageFeed packageFeed;
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
        try
        {
            logger.info("Downloading NPM changes feed for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            service = service.path(packageId);

            InputStream inputStream = service.request().buildGet().invoke(InputStream.class);
            packageFeed = rpmJacksonMapper.readValue(inputStream, PackageFeed.class);

            logger.info("Downloaded NPM changes feed for [{}].", remoteRepository.getUrl());

        }
        catch (Exception e)
        {
            logger.error("Failed to fetch NPM changes feed [{}]", remoteRepositoryUrl, e);
            return;
        }
        finally
        {
            restClient.close();
        }

        try
        {
            rpmPackageFeedParser.parseFeed(repository, packageFeed);
        }
        catch (Exception e)
        {
            logger.error("Failed to parse NPM feed [{}/{}]",
                    ((RepositoryData)repository).getRemoteRepository().getUrl(),
                    packageFeed.getName(),
                    e);
        }
    }

    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class RpmSearchPackagesEventListener
    {

        private RpmSearchRequest rpmSearchRequest;

        public RpmSearchRequest getRpmSearchRequest()
        {
            return rpmSearchRequest;
        }

        public void setRpmSearchRequest(RpmSearchRequest npmSearchRequest)
        {
            this.rpmSearchRequest = npmSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event)
        {
            if (rpmSearchRequest == null)
            {
                return;
            }

            String storageId = event.getStorageId();
            String repositoryId = event.getRepositoryId();

            Storage storage = getConfiguration().getStorage(storageId);
            Repository repository = storage.getRepository(repositoryId);
            RemoteRepository remoteRepository = repository.getRemoteRepository();
            if (remoteRepository == null)
            {
                return;
            }

            RepositorySearchRequest predicate = event.getPredicate();
            Boolean packageExists = packagesExists(storageId, repositoryId, predicate);

            logger.info("RPM remote repository [{}] cached package existance is [{}]",
                    repository.getId(), packageExists);

            Runnable job = () -> fetchRemoteSearchResult(storageId, repositoryId, rpmSearchRequest.getText(),
                    rpmSearchRequest.getSize());
            if (Boolean.FALSE.equals(packageExists))
            {
                // Syncronously fetch remote package feed if ve have no cached
                // packages
                job.run();
            }
            else
            {
                eventTaskExecutor.execute(job);
            }

        }
    }

    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class RpmViewPackageEventListener
    {

        private RpmViewRequest rpmSearchRequest;

        public RpmViewRequest getRpmSearchRequest()
        {
            return rpmSearchRequest;
        }

        public void setNpmSearchRequest(RpmViewRequest rpmSearchRequest)
        {
            this.rpmSearchRequest = rpmSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event)
        {
            if (rpmSearchRequest == null)
            {
                return;
            }

            String storageId = event.getStorageId();
            String repositoryId = event.getRepositoryId();

            Storage storage = getConfiguration().getStorage(storageId);
            Repository repository = storage.getRepository(repositoryId);
            RemoteRepository remoteRepository = repository.getRemoteRepository();
            if (remoteRepository == null)
            {
                return;
            }

            RepositorySearchRequest predicate = event.getPredicate();
            Boolean packagesExists = packagesExists(storageId, repositoryId, predicate);

            logger.info("NPM remote repository [{}] cached package ixistance is [{}]",
                    repository.getId(), packagesExists);

            Runnable job = () -> fetchRemotePackageFeed(storage.getId(), repository.getId(),
                    rpmSearchRequest.getPackageId());
            if (!Boolean.TRUE.equals(packagesExists))
            {
                // Synchronously fetch remote package feed if there is no cached packages
                job.run();
            }
            else
            {
                eventTaskExecutor.execute(job);
            }
        }

    }

    private Boolean packagesExists(String storageId,
                                   String repositoryId,
                                   RepositorySearchRequest predicate)
    {
        return artifactIdGroupRepository.commonArtifactsExists(storageId, repositoryId,
                predicate.getArtifactId(),
                predicate.getUseArtifactName(),
                predicate.getCoordinateValues());
    }

    protected Configuration getConfiguration()
    {
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
}
