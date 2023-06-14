package com.veadan.folib.repository;

import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.domain.PypiPackageInfo;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.pypi.PypiSearchRequest;
import com.veadan.folib.pypi.PypiSearchResult;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.veadan.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.veadan.folib.storage.validation.deployment.RedeploymentValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

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
        implements RepositoryFeatures
{

    private static final Logger logger = LoggerFactory.getLogger(PypiRepositoryFeatures.class);
    private static final Pattern PACKAGE_NAME_PATTERN = Pattern.compile(PypiPackageInfo.NAME_FORMAT);
    @Inject
    private ConfigurationManager configurationManager;
    @Inject
    private ArtifactIdGroupRepository artifactIdGroupRepository;
    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;
    @Inject
    private PypiPackageFeedParser pypiPackageFeedParser;
    @Inject
    private Executor eventTaskExecutor;
    @Inject
    private RedeploymentValidator redeploymentValidator;

    @Inject
    private GenericReleaseVersionValidator genericReleaseVersionValidator;

    @Inject
    private GenericSnapshotVersionValidator genericSnapshotVersionValidator;

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
        String targetUrl = String.format("%s/%s", remoteRepository.getUrl(), pypiSearchRequest.getPackageName());
        Client restClient = null;
        Response response = null;
        List<PypiSearchResult> pypiSearchResult;
        try {
            restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            logger.info("Search Pypi packages for [{}].", targetUrl);
            WebTarget service = restClient.target(targetUrl);
            response = service.request(MediaType.TEXT_HTML).get();
            String responseBodyStr = response.readEntity(String.class);
            pypiSearchResult = extractSearchResult(responseBodyStr);
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

    private Boolean packagesExists(String storageId,
                                   String repositoryId,
                                   RepositorySearchRequest predicate) {
        return artifactIdGroupRepository.commonArtifactsExists(storageId, repositoryId,
                predicate.getArtifactId(),
                predicate.getCoordinateValues());
    }

    private List<PypiSearchResult> extractSearchResult(String pypiSearchResult) {
        Matcher matcher = PACKAGE_NAME_PATTERN.matcher(pypiSearchResult);
        return matcher.results()
                .map(matchResult -> {
                    String artifactName = matchResult.group(2);
                    String artifactUrl = matchResult.group(1);
                    return new PypiSearchResult(artifactName, artifactUrl);
                })
                .collect(Collectors.toList());
    }

    protected Configuration getConfiguration() {
        return configurationManager.getConfiguration();
    }

}
