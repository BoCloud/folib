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

import com.folib.config.CustomAuthenticationFeature;
import com.folib.configuration.ConfigurationManager;
import com.folib.data.criteria.Paginator;
import com.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.folib.repositories.ArtifactRepository;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ArtifactTagService;
import com.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.folib.storage.validation.deployment.RedeploymentValidator;
import com.folib.configuration.Configuration;
import com.folib.nuget.NugetSearchRequest;
import com.folib.providers.repository.RepositorySearchRequest;
import com.folib.repositories.ArtifactIdGroupRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.storage.Storage;
import com.folib.storage.metadata.nuget.rss.PackageFeed;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.yaml.configuration.repository.NugetRepositoryConfiguration;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import java.io.IOException;
import java.util.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * 
 * @author Veadan
 * @author @author veadan
 */
@Component
public class NugetRepositoryFeatures
        implements RepositoryFeatures
{

    private static final int REMOTE_FEED_PAGE_SIZE = 1000;

    private static final Logger logger = LoggerFactory.getLogger(NugetRepositoryFeatures.class);

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private RedeploymentValidator redeploymentValidator;

    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;

    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;

    @Inject
    private ArtifactIdGroupService artifactIdGroupService;
    
    private Set<String> defaultMavenArtifactCoordinateValidators;

    @Inject
    private ArtifactRepository artifactRepository;

    @PostConstruct
    public void init()
    {
        defaultMavenArtifactCoordinateValidators = new LinkedHashSet<>(Arrays.asList(redeploymentValidator.getAlias(),
                                                                                     genericReleaseVersionValidator.getAlias(),
                                                                                     genericSnapshotVersionValidator.getAlias()));
    }

    public void downloadRemoteFeed(String storageId,
                                   String repositoryId)
            throws IOException
    {
        downloadRemoteFeed(storageId, repositoryId, new NugetSearchRequest());
    }

    public void downloadRemoteFeed(String storageId,
                                   String repositoryId,
                                   NugetSearchRequest nugetSearchRequest)
            throws IOException
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        Optional<NugetRepositoryConfiguration> repositoryConfiguration = Optional.ofNullable((NugetRepositoryConfiguration) repository.getRepositoryConfiguration());
        Integer remoteFeedPageSize = repositoryConfiguration.map(c -> c.getRemoteFeedPageSize())
                                                            .orElse(REMOTE_FEED_PAGE_SIZE);

        for (int i = 0; true; i++)
        {
            if (!downloadRemoteFeed(storageId,
                                    repositoryId,
                                    nugetSearchRequest,
                                    i * remoteFeedPageSize,
                                    remoteFeedPageSize))
            {
                break;
            }
        }
    }

    public boolean downloadRemoteFeed(String storageId,
                                      String repositoryId,
                                      NugetSearchRequest nugetSearchRequest,
                                      long skip,
                                      int top)
    {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null)
        {
            return false;
        }
        String remoteRepositoryUrl = remoteRepository.getUrl();

        Paginator paginator = new Paginator();
        paginator.setLimit(top);
        paginator.setSkip(skip);

        PackageFeed packageFeed;
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId,repositoryId);
        try
        {
            logger.info("Downloading remote feed for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            packageFeed = queryParams(service.path("Search()"), nugetSearchRequest, paginator).request()
                                                                                              .buildGet()
                                                                                              .invoke(PackageFeed.class);

//            logger.info("Downloaded remote feed for [{}], size [{}].",
//                         remoteRepository.getUrl(),
//                         Optional.of(packageFeed).map(f -> f.getEntries().size()).orElse(0));

        }
        catch (Exception e)
        {
            logger.error("Failed to fetch Nuget remote feed [{}]", remoteRepositoryUrl, e);
            return false;
        }
        finally
        {
            restClient.close();
        }

//        if (packageFeed == null || packageFeed.getEntries() == null || packageFeed.getEntries().size() == 0)
//        {
//            return false;
//        }

        parseFeed(repository, packageFeed);

        return true;
    }

    private void parseFeed(Repository repository,
                           PackageFeed packageFeed)
    {
//        String repositoryId = repository.getId();
//        String storageId = repository.getStorage().getId();
//
//        ArtifactTag lastVersionTag = artifactTagService.findOneOrCreate(ArtifactTagEntity.LAST_VERSION);
//
//        Set<Artifact> artifactToSaveSet = new HashSet<>();
//        for (PackageEntry packageEntry : packageFeed.getEntries())
//        {
//            String packageId = packageEntry.getProperties().getId();
//            packageId = packageId == null ? packageEntry.getTitle() : packageId;
//            String packageVersion = packageEntry.getProperties().getVersion().toString();
//
//            NugetArtifactCoordinates c = new NugetArtifactCoordinates(packageId, packageVersion, "nupkg");
//
//            LocalDateTime now = LocalDateTimeInstance.now();
//
//            Artifact artifact = artifactRepository.findOneArtifact(storageId, repositoryId, c.buildPath());
//            ArtifactEntity remoteArtifactEntry = null;
//            if (Objects.nonNull(artifact)) {
//                //已存在
//                remoteArtifactEntry = new ArtifactEntity(artifact.getNativeId(), storageId, repositoryId, artifact.getUuid(), c);
//            } else {
//                //不存在
//                remoteArtifactEntry = new ArtifactEntity(storageId, repositoryId, c);
//                remoteArtifactEntry.setStorageId(storageId);
//                remoteArtifactEntry.setRepositoryId(repositoryId);
//                remoteArtifactEntry.setArtifactCoordinates(c);
//                remoteArtifactEntry.setLastUsed(now);
//                remoteArtifactEntry.setLastUpdated(now);
//                remoteArtifactEntry.setDownloadCount(0);
//                remoteArtifactEntry.setArtifactFileExists(Boolean.FALSE);
//            }
//
//            remoteArtifactEntry.setSizeInBytes(packageEntry.getProperties().getPackageSize());
//
//            if (Boolean.TRUE.equals(packageEntry.getProperties().getIsLatestVersion()))
//            {
//                remoteArtifactEntry.getTagSet().add(lastVersionTag);
//            }
//
//            artifactToSaveSet.add(remoteArtifactEntry);
//        }
//
//        artifactIdGroupService.saveArtifacts(repository, artifactToSaveSet);
    }


    protected Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class RepositorySearchEventListener
    {

        private NugetSearchRequest nugetSearchRequest;

        public NugetSearchRequest getNugetSearchRequest()
        {
            return nugetSearchRequest;
        }

        public void setNugetSearchRequest(NugetSearchRequest nugetSearchRequest)
        {
            this.nugetSearchRequest = nugetSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event)
        {
            if (nugetSearchRequest == null)
            {
                return;
            }
            
            Storage storage = getConfiguration().getStorage(event.getStorageId());
            Repository repository = storage.getRepository(event.getRepositoryId());
            RemoteRepository remoteRepository = repository.getRemoteRepository();
            if (remoteRepository == null)
            {
                return;
            }

            RepositorySearchRequest predicate = event.getPredicate();
            String repositoryId = event.getRepositoryId();
            String storageId = event.getStorageId();
            Long packageCount = artifactIdGroupRepository.commonCountArtifacts(storageId, repositoryId,
                                                                         predicate.getArtifactId(),
                                                                         predicate.getCoordinateValues());

            logger.info("Remote repository [{}] cached package count is [{}]", repository.getId(), packageCount);

            Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            PackageFeed feed;
            try
            {
                WebTarget service = restClient.target(remoteRepository.getUrl());
                authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
                Long remotePackageCount = Long.valueOf(queryParams(service.path("Search()/$count"),
                                                                   nugetSearchRequest, new Paginator()).request()
                                                                                                       .buildGet()
                                                                                                       .invoke(String.class));
                logger.info("Remote repository [{}] remote package count is [{}]", repository.getId(), remotePackageCount);

                if (Long.valueOf(remotePackageCount).compareTo(packageCount) == 0)
                {
                    logger.info("No need to download remote feed, there was no changes in remote repository [{}] against local cache.",
                                 remoteRepository.getUrl());
                    return;
                }

                logger.info("Downloading remote feed for [{}].", remoteRepository.getUrl());

                feed = queryParams(service.path("Search()"), nugetSearchRequest, event.getPaginator()).request()
                                                                                                      .buildGet()
                                                                                                      .invoke(PackageFeed.class);

//                logger.info("Downloaded remote feed for [{}], size [{}].",
//                             remoteRepository.getUrl(),
//                             Optional.of(feed).map(f -> f.getEntries().size()).orElse(0));

            }
            catch (Exception e)
            {
                logger.error("Failed to fetch Nuget remote feed [{}]", remoteRepository.getUrl(), e);
                return;
            } 
            finally
            {
                restClient.close();
            }

            parseFeed(repository, feed);
        }

    }

    private WebTarget queryParams(WebTarget path,
                                  NugetSearchRequest nugetSearchRequest,
                                  Paginator paginator)
    {
        if (nugetSearchRequest.getFilter() != null && !nugetSearchRequest.getFilter().trim().isEmpty())
        {
            path = path.queryParam("$filter", nugetSearchRequest.getFilter().trim());
        }
        if (nugetSearchRequest.getSearchTerm() != null && !nugetSearchRequest.getSearchTerm().trim().isEmpty())
        {
            path = path.queryParam("searchTerm", nugetSearchRequest.getSearchTerm().trim());
        }
        if (nugetSearchRequest.getTargetFramework() != null
                && !nugetSearchRequest.getTargetFramework().trim().isEmpty())
        {
            path = path.queryParam("targetFramework", nugetSearchRequest.getTargetFramework().trim());
        }

        if (paginator.getSkip() != null && paginator.getSkip() > 0)
        {
            path = path.queryParam("$skip", paginator.getSkip());
        }
        if (paginator.getLimit() != null && paginator.getLimit() > 0)
        {
            path = path.queryParam("$top", paginator.getLimit());
        }

        //https://chocolatey.org/api/v2/$metadata 
        // includePrerelease is required parameter to invoke Search() method
        path = path.queryParam("includePrerelease", Boolean.TRUE.equals(nugetSearchRequest.getIncludePreRelease()));

        return path;
    }

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators()
    {
        return defaultMavenArtifactCoordinateValidators;
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
