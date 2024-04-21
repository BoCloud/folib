package com.veadan.folib.repository;

import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.config.NpmLayoutProviderConfig.NpmObjectMapper;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.domain.ArtifactIdGroup;
import com.veadan.folib.domain.ArtifactIdGroupEntity;
import com.veadan.folib.npm.NpmSearchRequest;
import com.veadan.folib.npm.NpmViewRequest;
import com.veadan.folib.npm.metadata.*;
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
import com.veadan.folib.util.LocalDateTimeInstance;
import com.veadan.folib.yaml.configuration.repository.NpmRepositoryConfigurationData;
import com.veadan.folib.yaml.configuration.repository.remote.NpmRemoteRepositoryConfiguration;
import com.veadan.folib.yaml.configuration.repository.remote.NpmRemoteRepositoryConfigurationDto;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.HttpStatus;
import org.apache.http.client.config.RequestConfig;
import org.folib.util.Commons;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
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
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.time.LocalDateTime;
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

        logger.debug("allowsUnpublish is [{}] for storageId: [{}]; repositoryId: [{}]",
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
            logger.debug("Search NPM packages for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            service = service.path("-/v1/search").queryParam("text", text).queryParam("size", size);

            InputStream inputStream = service.request().buildGet().invoke(InputStream.class);
            searchResults = npmJacksonMapper.readValue(inputStream, SearchResults.class);

            logger.debug("Searched NPM packages for [{}].", remoteRepository.getUrl());

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
            logger.debug("Fetching remote changes for [{}] since [{}].", replicateUrl, since);

            WebTarget service = restClient.target(replicateUrl);
            authentication(service, repository.getRemoteRepository().getUsername(), repository.getRemoteRepository().getPassword());
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

        logger.debug("Fetched remote changes for  [{}] since [{}].",
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
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            service = service.path(packageId);
            url = service.getUri().toString();
            logger.debug("Downloading NPM remote package feed for [{}].", url);
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
            logger.debug("Downloaded NPM remote package feed for [{}] take time [{}] ms.", url, System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            logger.error("Failed to fetch NPM remote package feed [{}]", url, e);
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

    public String fetchRemoteBinaryFeed(String storageId,
                                        String repositoryId,
                                        String packageId) {
        String binaryFeed = null;
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
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            service = service.path(packageId);
            url = service.getUri().toString();
            logger.debug("Downloading NPM remote binary feed for [{}].", url);
            response = service.request(MediaType.APPLICATION_JSON).get();
            if (response.getStatus() == HttpStatus.SC_OK) {
                String readString = response.readEntity(String.class);
                if (StringUtils.isNotBlank(readString)) {
                    binaryFeed = readString;
                }
            } else {
                displayResponseError(url, response);
                return null;
            }
            logger.debug("Downloaded NPM remote binary feed for [{}] take time [{}] ms.", url, System.currentTimeMillis() - startTime);
        } catch (Exception e) {
            logger.error("Failed to fetch NPM remote binary feed [{}]", url, e);
            return binaryFeed;
        } finally {
            if (Objects.nonNull(response)) {
                response.close();
            }
        }
        return binaryFeed;
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

            logger.debug("NPM remote repository [{}] local cached package count is [{}]",
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
            handleViewPackage(event.getStorageId(), event.getRepositoryId(), event.getPredicate().getArtifactId());
        }
    }

    public PackageFeed handleViewPackage(String storageId, String repositoryId, String packageId) {
        PackageFeed packageFeed = null;
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return packageFeed;
        }
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, packageId);
        packageFeed = fetchRemotePackageFeed(storage.getId(), repository.getId(),
                packageId);
        if (Objects.nonNull(packageFeed)) {
            String separator = "/";
            String baseUrl = StringUtils.chomp(configurationManager.getConfiguration().getBaseUrl(), separator);
            String repositoryBaseUrl = baseUrl + String.format("/storages/%s/%s/", storageId, repositoryId);
            logger.debug("[{}] storage [{}] repository [{}] repositoryBaseUrl is [{}]", this.getClass().getSimpleName(), storageId, repositoryId, repositoryBaseUrl);
            Versions versions = packageFeed.getVersions();
            if (Objects.nonNull(versions) && MapUtils.isNotEmpty(versions.getAdditionalProperties())) {
                long startTime = System.currentTimeMillis();
                NpmArtifactCoordinates npmArtifactCoordinates = null;
                URI uri = null;
                for (Map.Entry<String, PackageVersion> versionEntry : versions.getAdditionalProperties().entrySet()) {
                    Dist dist = versionEntry.getValue().getDist();
                    if (Objects.nonNull(dist) && StringUtils.isNotBlank(dist.getTarball())) {
                        npmArtifactCoordinates = NpmArtifactCoordinates.of(versionEntry.getValue().getName(), versionEntry.getValue().getVersion());
                        uri = npmArtifactCoordinates.convertToResource(npmArtifactCoordinates);
                        dist.setTarball(repositoryBaseUrl + uri.toString());
                    }
                }
                logger.debug("[{}] storage [{}] repository [{}] replace tarball take time [{}] ms", this.getClass().getSimpleName(), storageId, repositoryId, System.currentTimeMillis() - startTime);
            }
        } else {
            //兼容代理源不能使用的情况
            artifactIdGroup = artifactIdGroupRepository.findByArtifactIdGroup(artifactIdGroup.getUuid());
            if (Objects.nonNull(artifactIdGroup)) {
                String metadata = getArtifactIdGroupMetadata(artifactIdGroup);
                if (StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata)) {
                    try (InputStream inputStream = new ByteArrayInputStream(metadata.getBytes())) {
                        packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);
                    } catch (IOException ex) {
                        logger.error("[{}] storage [{}] repository [{}] artifactIdGroup [{}] metadata to packageFeed error [{}]", this.getClass().getSimpleName(), storageId, repositoryId, artifactIdGroup.getUuid(), ExceptionUtils.getStackTrace(ex));
                    }
                }
            }
        }
        return packageFeed;
    }

    public String handleViewBinary(Repository sourceRepository, String storageId, String repositoryId, String packageId) {
        String binaryFeed = null;
        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return binaryFeed;
        }
        ArtifactIdGroup artifactIdGroup = new ArtifactIdGroupEntity(storageId, repositoryId, packageId);
        binaryFeed = fetchRemoteBinaryFeed(storage.getId(), repository.getId(),
                packageId);
        if (StringUtils.isBlank(binaryFeed)) {
            //兼容代理源不能使用的情况
            artifactIdGroup = artifactIdGroupRepository.findByArtifactIdGroup(artifactIdGroup.getUuid());
            if (Objects.nonNull(artifactIdGroup)) {
                String metadata = getArtifactIdGroupMetadata(artifactIdGroup);
                if (StringUtils.isNotBlank(metadata) && JSONUtil.isJson(metadata)) {
                    binaryFeed = metadata;
                }
            }
        } else if (JSONUtil.isJson(binaryFeed)) {
            String baseUrl = StringUtils.removeEnd(configurationManager.getConfiguration().getBaseUrl(), GlobalConstants.SEPARATOR);
            String repositoryBaseUrl = baseUrl + String.format("/storages/%s/%s", sourceRepository.getStorage().getId(), sourceRepository.getId());
            logger.debug("[{}] storage [{}] repository [{}] repositoryBaseUrl is [{}]", this.getClass().getSimpleName(), storageId, repositoryId, repositoryBaseUrl);
            JSONArray jsonArray = JSONArray.parseArray(binaryFeed);
            JSONObject jsonObject;
            String key = "url";
            URL url;
            for (int i = 0; i < jsonArray.size(); i++) {
                try {
                    jsonObject = jsonArray.getJSONObject(i);
                    if (jsonObject.containsKey(key)) {
                        url = new URL(jsonObject.getString(key));
                        jsonObject.put(key, repositoryBaseUrl + url.getPath());
                    }
                } catch (Exception ex) {
                    logger.warn(ExceptionUtils.getStackTrace(ex));
                }
            }
            binaryFeed = jsonArray.toJSONString();
        }
        return binaryFeed;
    }

    /**
     * 获取metadata
     *
     * @param artifactIdGroup artifactIdGroup
     * @return metadata
     */
    public String getArtifactIdGroupMetadata(ArtifactIdGroup artifactIdGroup) {
        if (Objects.isNull(artifactIdGroup) || StringUtils.isBlank(artifactIdGroup.getMetadata()) || !JSONUtil.isJson(artifactIdGroup.getMetadata())) {
            return "";
        }
        JSONObject metadataJson = JSONObject.parseObject(artifactIdGroup.getMetadata());
        String cacheTimeKey = "cacheTime", metadataKey = "metadata";
        if (metadataJson.containsKey(cacheTimeKey)) {
            String data = metadataJson.getString(metadataKey);
            return GlobalConstants.NO_DATA.equals(data) ? "" : data;
        }
        return "";
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

    /**
     * Client WebTarget 构建认证信息
     *
     * @param webTarget webTarget
     * @param username  username
     * @param password  password
     */
    public void authentication(WebTarget webTarget, String username, String password) {
        final HttpAuthenticationFeature authenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? HttpAuthenticationFeature.basic(username, password) : null;
        if (authenticationFeature != null) {
            webTarget.register(authenticationFeature);
            webTarget.property(ApacheClientProperties.REQUEST_CONFIG,
                    RequestConfig.custom().setCircularRedirectsAllowed(true).build());
        }
    }
}
