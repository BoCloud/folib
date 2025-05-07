package com.veadan.folib.repository;

import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.php.PhpSearchRequest;
import com.veadan.folib.php.PhpSearchResult;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ArtifactResolutionService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Executor;

/**
 * @author leipenghui
 */
@Slf4j
@Component
public class PhpRepositoryFeatures
        implements RepositoryFeatures {

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
    private Executor eventTaskExecutor;
    @Lazy
    @Inject
    private PhpPackageFeedParser phpPackageFeedParser;
    @Lazy
    @Inject
    protected ArtifactResolutionService artifactResolutionService;
    @Lazy
    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    private Set<String> defaultArtifactCoordinateValidators = new LinkedHashSet<>();

    @Override
    public Set<String> getDefaultArtifactCoordinateValidators() {
        return defaultArtifactCoordinateValidators;
    }

    @Component
    @Scope(scopeName = "request", proxyMode = ScopedProxyMode.TARGET_CLASS)
    public class PhpSearchPackagesEventListener {

        private PhpSearchRequest phpSearchRequest;

        public PhpSearchRequest getPhpSearchRequest() {
            return phpSearchRequest;
        }

        public void setPhpSearchRequest(PhpSearchRequest phpSearchRequest) {
            this.phpSearchRequest = phpSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event) {
            if (phpSearchRequest == null) {
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

            log.info("PHP remote repository [{}] cached package existance is [{}]",
                    repository.getId(), packageExists);

            Runnable job = () -> fetchRemoteSearchResult(storageId, repositoryId, phpSearchRequest);
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
                                         PhpSearchRequest phpSearchRequest) {

        Storage storage = getConfiguration().getStorage(storageId);
        Repository repository = storage.getRepository(repositoryId);
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (remoteRepository == null) {
            return;
        }
        if (!remoteRepositoryAlivenessCacheManager.isAlive(remoteRepository)) {
            log.warn("Remote storageId [{}] repositoryId [{}] url [{}] is down.", storageId, repositoryId, remoteRepository.getUrl());
            return;
        }
        String targetUrl = phpSearchRequest.getTargetUrl();
        PhpSearchResult phpSearchResult;
        Client restClient = null;
        Response response = null;
        try {
            restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            log.info("Search PHP packages for [{}].", targetUrl);
            WebTarget service = restClient.target(phpSearchRequest.getTargetUrl());
            authentication(service, remoteRepository.getUsername(), remoteRepository.getPassword());
            service = service.queryParam("q", phpSearchRequest.getQ()).queryParam("type", phpSearchRequest.getType());
            response = service.request(MediaType.APPLICATION_JSON).get();
            phpSearchResult = response.readEntity(PhpSearchResult.class);
            log.info("Searched PHP packages for [{}].", targetUrl);
        } catch (Exception e) {
            log.error("Failed to search PHP packages [{}]", targetUrl, e);
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
            phpPackageFeedParser.parseSearchResult(repository, phpSearchResult);
        } catch (Exception e) {
            log.error("Failed to parse PHP packages search result for [{}]", targetUrl, e);
        }
    }

    private Boolean packagesExists(String storageId,
                                   String repositoryId,
                                   RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonArtifactsExists(storageId, repositoryId,
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
