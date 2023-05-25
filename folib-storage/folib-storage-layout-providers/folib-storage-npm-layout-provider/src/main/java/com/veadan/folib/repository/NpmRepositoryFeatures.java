package com.veadan.folib.repository;

import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.config.NpmLayoutProviderConfig.NpmObjectMapper;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.npm.NpmSearchRequest;
import com.veadan.folib.npm.NpmViewRequest;
import com.veadan.folib.npm.metadata.*;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.io.RepositoryPathResolver;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactManagementService;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.veadan.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.veadan.folib.storage.validation.deployment.RedeploymentValidator;
import com.veadan.folib.yaml.configuration.repository.NpmRepositoryConfigurationData;
import com.veadan.folib.yaml.configuration.repository.remote.NpmRemoteRepositoryConfiguration;
import com.veadan.folib.yaml.configuration.repository.remote.NpmRemoteRepositoryConfigurationDto;
import org.apache.commons.collections4.MapUtils;
import org.apache.http.HttpStatus;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;
import org.springframework.web.util.UriComponentsBuilder;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.*;
import java.util.concurrent.Executor;

@Component
public class NpmRepositoryFeatures implements RepositoryFeatures {

    private static final int CHANGES_BATCH_SIZE = 500;

    private static final boolean ALLOWS_UNPUBLISH_DEFAULT = true;

    private static final Logger logger = LoggerFactory.getLogger(NpmRepositoryFeatures.class);

    @Inject
    private ConfigurationManagementService configurationManagementService;

    @Inject
    private RedeploymentValidator redeploymentValidator;

    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;

    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;

    @Inject
    private Executor eventTaskExecutor;

    @Inject
    @NpmObjectMapper
    private ObjectMapper npmJacksonMapper;

    @Inject
    private NpmPackageFeedParser npmPackageFeedParser;

    @Inject
    protected RepositoryPathResolver repositoryPathResolver;

    private Set<String> defaultArtifactCoordinateValidators;

    @Inject
    @Lazy
    private ArtifactManagementService artifactManagementService;

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

    public boolean allowsUnpublish(String storageId,
                                   String repositoryId) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        Optional<NpmRepositoryConfigurationData> repositoryConfiguration = Optional.ofNullable(
                (NpmRepositoryConfigurationData) repository.getRepositoryConfiguration());
        boolean allowsUnpublish = repositoryConfiguration.map(NpmRepositoryConfigurationData::isAllowsUnpublish)
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
                                         Integer size) {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return;
        }
        String remoteRepositoryUrl = remoteRepository.getUrl();

        SearchResults searchResults;
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
        try {
            logger.info("Search NPM packages for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            service = service.path("-/v1/search").queryParam("text", text).queryParam("size", size);

            InputStream inputStream = service.request().buildGet().invoke(InputStream.class);
            searchResults = npmJacksonMapper.readValue(inputStream, SearchResults.class);

            logger.info("Searched NPM packages for [{}].", remoteRepository.getUrl());

        } catch (Exception e) {
            logger.error("Failed to search NPM packages [{}]", remoteRepositoryUrl, e);

            return;
        } finally {
            restClient.close();
        }

        try {
            npmPackageFeedParser.parseSearchResult(repository, searchResults);
        } catch (Exception e) {
            logger.error("Failed to parse NPM packages search result for [{}]", remoteRepositoryUrl, e);
        }
    }

    public void fetchRemoteChangesFeed(String storageId,
                                       String repositoryId)
            throws IOException {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return;
        }

        RepositoryDto mutableRepository = configurationManagementService.getMutableConfigurationClone()
                .getStorage(storageId)
                .getRepository(repositoryId);
        NpmRemoteRepositoryConfigurationDto mutableConfiguration = (NpmRemoteRepositoryConfigurationDto) mutableRepository.getRemoteRepository()
                .getCustomConfiguration();

        NpmRemoteRepositoryConfiguration configuration = (NpmRemoteRepositoryConfiguration) remoteRepository.getCustomConfiguration();
        if (configuration == null) {
            logger.warn("Remote npm configuration not found for [{}]/[{}]", storageId, repositoryId);
            return;
        }
        Long lastCnahgeId = configuration.getLastChangeId();
        String replicateUrl = configuration.getReplicateUrl();

        Long nextChangeId = lastCnahgeId;
        do {
            lastCnahgeId = nextChangeId;
            mutableConfiguration.setLastChangeId(nextChangeId);
            configurationManagementService.saveRepository(storageId, mutableRepository);

            nextChangeId = Long.valueOf(fetchRemoteChangesFeed(repository, replicateUrl, lastCnahgeId + 1));
        } while (nextChangeId > lastCnahgeId);
    }

    private Integer fetchRemoteChangesFeed(Repository repository,
                                           String replicateUrl,
                                           Long since)
            throws IOException {
        int result = 0;
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.
                getRestClient(repository.getStorage().getId(), repository.getId());
        try {
            logger.info("Fetching remote changes for [{}] since [{}].", replicateUrl, since);

            WebTarget service = restClient.target(replicateUrl);
            service = service.path("_changes");
            service = service.queryParam("since", since);
            service = service.queryParam("include_docs", true);
            service = service.queryParam("limit", CHANGES_BATCH_SIZE);

            Invocation request = service.request().buildGet();

            result = fetchRemoteChangesFeed(repository, request);
        } finally {
            restClient.close();
        }

        return result;
    }

    private int fetchRemoteChangesFeed(Repository repository,
                                       Invocation request)
            throws IOException {
        int result = 0;

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        NpmRemoteRepositoryConfiguration repositoryConfiguration = (NpmRemoteRepositoryConfiguration) remoteRepository.getCustomConfiguration();

        JsonFactory jfactory = new JsonFactory();

        try (InputStream is = request.invoke(InputStream.class)) {

            JsonParser jp = jfactory.createParser(is);
            jp.setCodec(npmJacksonMapper);

            Assert.isTrue(jp.nextToken() == JsonToken.START_OBJECT, "npm changes feed should be JSON object.");
            Assert.isTrue(jp.nextFieldName().equals("results"), "npm changes feed should contains `results` field.");
            Assert.isTrue(jp.nextToken() == JsonToken.START_ARRAY, "npm changes feed `results` should be array.");

            StringBuffer sb = new StringBuffer();
            while (jp.nextToken() != null) {
                JsonToken nextToken = jp.currentToken();
                if (nextToken == JsonToken.END_ARRAY) {
                    break;
                }

                JsonNode node = jp.readValueAsTree();
                sb.append(node.toString());

                String changeValue = sb.toString();

                Change change;
                try {
                    change = npmJacksonMapper.readValue(changeValue, Change.class);
                } catch (Exception e) {
                    logger.error("Failed to parse NPM changes feed [{}] since [{}]: \n {}",
                            repositoryConfiguration.getReplicateUrl(),
                            repositoryConfiguration.getLastChangeId(),
                            changeValue,
                            e);

                    return result;
                }

                PackageFeed packageFeed = change.getDoc();
                try {
                    npmPackageFeedParser.parseFeed(repository, packageFeed);
                } catch (Exception e) {
                    logger.error("Failed to parse NPM feed [{}/{}]",
                            ((RepositoryData) repository).getRemoteRepository().getUrl(),
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

    public PackageFeed fetchRemotePackageFeed(String storageId,
                                              String repositoryId,
                                              String packageId) {
        PackageFeed packageFeed = null;
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);

        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return null;
        }
        String url = "";
        long startTime = System.currentTimeMillis();
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
        Response response = null;
        try {
            WebTarget service = restClient.target(remoteRepository.getUrl());
            service = service.path(packageId);
            url = service.getUri().toString();
            logger.info("Downloading NPM changes feed for [{}].", url);
            response = service.request(MediaType.APPLICATION_JSON).get();
            if (response.getStatus() == HttpStatus.SC_OK) {
                String readString = response.readEntity(String.class);
                try (InputStream inputStream = new ByteArrayInputStream(readString.getBytes())) {
                    packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);
                }
            } else {
                displayResponseError(url, response);
                return null;
            }
            logger.info("Downloaded NPM changes feed for [{}] take time [{}] ms.", url, System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            logger.error("Failed to fetch NPM changes feed [{}]", url, e);
            return packageFeed;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        return packageFeed;
    }

    public void npmPackageFeed(Repository repository, PackageFeed packageFeed) {
        try {
            npmPackageFeedParser.parseFeed(repository, packageFeed);
        } catch (Exception e) {
            logger.error("Failed to parse NPM feed [{}/{}]",
                    ((RepositoryData) repository).getRemoteRepository().getUrl(),
                    packageFeed.getName(),
                    e);
        }
    }

    /**
     * 返回错误信息
     *
     * @param url      url
     * @param response response
     */
    public static void displayResponseError(String url, Response response) {
        logger.error("url {} Status code {}", url, response.getStatus());
        logger.error("url {} Status info {}", url, response.getStatusInfo().getReasonPhrase());
        logger.error("url {} Response message {}", url, response.readEntity(String.class));
        logger.error(response.toString());
    }

    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class SearchPackagesEventListener {

        private NpmSearchRequest npmSearchRequest;

        public NpmSearchRequest getNpmSearchRequest() {
            return npmSearchRequest;
        }

        public void setNpmSearchRequest(NpmSearchRequest npmSearchRequest) {
            this.npmSearchRequest = npmSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event) {
            if (npmSearchRequest == null) {
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
            Long packagesCount = packagesCount(storageId, repositoryId, predicate);

            logger.info("NPM remote repository [{}] local cached package count is [{}]",
                    repository.getId(), packagesCount);

            Runnable job = () -> fetchRemoteSearchResult(storageId, repositoryId, npmSearchRequest.getText(),
                    npmSearchRequest.getSize());
            job.run();
//            if (Boolean.FALSE.equals(packageExists)) {
//                // Syncronously fetch remote package feed if ve have no cached
//                // packages
//                job.run();
//            } else {
//                eventTaskExecutor.execute(job);
//            }

        }
    }

    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class ViewPackageEventListener {

        private NpmViewRequest npmSearchRequest;

        public NpmViewRequest getNpmSearchRequest() {
            return npmSearchRequest;
        }

        public void setNpmSearchRequest(NpmViewRequest npmSearchRequest) {
            this.npmSearchRequest = npmSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event) {
            if (npmSearchRequest == null) {
                return;
            }
            if (!npmSearchRequest.getPackageId().equals(event.getPredicate().getArtifactId())) {
                return;
            }
            handleViewPackage(event.getStorageId(), event.getRepositoryId(), event.getPredicate().getArtifactId(), event.getPredicate(), null);
        }

    }

    public void handleViewPackage(String storageId, String repositoryId, String packageId, RepositorySearchRequest predicate, Boolean isAsync) {
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return;
        }
        Long packagesCount = packagesCount(storageId, repositoryId, predicate);
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, predicate.getArtifactId());
        PackageFeed packageFeed = fetchRemotePackageFeed(storage.getId(), repository.getId(),
                packageId);
        int remotePackagesCount = 0;
        if (Objects.nonNull(packageFeed)) {
            Versions versions = packageFeed.getVersions();
            if (Objects.nonNull(versions) && MapUtils.isNotEmpty(versions.getAdditionalProperties())) {
                remotePackagesCount = versions.getAdditionalProperties().size();
            }
            if (Objects.isNull(isAsync) && packagesCount > 0 && remotePackagesCount == packagesCount.intValue()) {
                //本地缓存大于0并且缓存数量等于远程代理仓库数量
                isAsync = true;
            }
        }
        logger.info("NPM remote repository [{}] package count is [{}] local cached package count is [{}] isAsync [{}]",
                artifactIdGroup.getUuid(), remotePackagesCount, packagesCount, Boolean.TRUE.equals(isAsync));
        Runnable job = () -> npmPackageFeed(repository, packageFeed);
        if (Boolean.TRUE.equals(isAsync)) {
            eventTaskExecutor.execute(job);
        } else {
            job.run();
        }
    }

    private Boolean packagesExists(String storageId,
                                   String repositoryId,
                                   RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonArtifactsExists(storageId, repositoryId,
                predicate.getArtifactId(),
                predicate.getCoordinateValues());
    }

    private Long packagesCount(String storageId,
                               String repositoryId,
                               RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonCountArtifacts(storageId, repositoryId,
                predicate.getArtifactId(),
                predicate.getCoordinateValues());
    }

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }
}
