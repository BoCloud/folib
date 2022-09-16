package com.veadan.folib.repository;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.Executor;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.repository.event.RemoteRepositorySearchEvent;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.validation.artifact.version.GenericReleaseVersionValidator;
import com.veadan.folib.storage.validation.artifact.version.GenericSnapshotVersionValidator;
import com.veadan.folib.storage.validation.deployment.RedeploymentValidator;
import com.veadan.folib.config.NpmLayoutProviderConfig.NpmObjectMapper;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.npm.NpmSearchRequest;
import com.veadan.folib.npm.NpmViewRequest;
import com.veadan.folib.npm.metadata.Change;
import com.veadan.folib.npm.metadata.PackageFeed;
import com.veadan.folib.npm.metadata.SearchResults;
import com.veadan.folib.providers.repository.RepositorySearchRequest;
import com.veadan.folib.repositories.ArtifactIdGroupRepository;
import com.veadan.folib.services.ConfigurationManagementService;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.yaml.configuration.repository.NpmRepositoryConfigurationData;
import com.veadan.folib.yaml.configuration.repository.remote.NpmRemoteRepositoryConfiguration;
import com.veadan.folib.yaml.configuration.repository.remote.NpmRemoteRepositoryConfigurationDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

@Component
public class NpmRepositoryFeatures implements RepositoryFeatures
{

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
            logger.debug("Search NPM packages for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            service = service.path("-/v1/search").queryParam("text", text).queryParam("size", size);

            InputStream inputStream = service.request().buildGet().invoke(InputStream.class);
            searchResults = npmJacksonMapper.readValue(inputStream, SearchResults.class);

            logger.debug("Searched NPM packages for [{}].", remoteRepository.getUrl());

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
            npmPackageFeedParser.parseSearchResult(repository, searchResults);
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
        NpmRemoteRepositoryConfigurationDto mutableConfiguration = (NpmRemoteRepositoryConfigurationDto) mutableRepository.getRemoteRepository()
                                                                                                                                  .getCustomConfiguration();

        NpmRemoteRepositoryConfiguration configuration = (NpmRemoteRepositoryConfiguration) remoteRepository.getCustomConfiguration();
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
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.
                getRestClient(repository.getStorage().getId(),repository.getId());//todo 修复
        try
        {
            logger.debug("Fetching remote changes for [{}] since [{}].", replicateUrl, since);

            WebTarget service = restClient.target(replicateUrl);
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
        NpmRemoteRepositoryConfiguration repositoryConfiguration = (NpmRemoteRepositoryConfiguration) remoteRepository.getCustomConfiguration();

        JsonFactory jfactory = new JsonFactory();

        try (InputStream is = request.invoke(InputStream.class))
        {

            JsonParser jp = jfactory.createParser(is);
            jp.setCodec(npmJacksonMapper);

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
                    change = npmJacksonMapper.readValue(changeValue, Change.class);
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
                    npmPackageFeedParser.parseFeed(repository, packageFeed);
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

        logger.debug("Fetched remote changes for  [{}] since [{}].",
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
        Client restClient = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId,repositoryId);
        try
        {
            logger.debug("Downloading NPM changes feed for [{}].", remoteRepositoryUrl);

            WebTarget service = restClient.target(remoteRepository.getUrl());
            service = service.path(packageId);

            InputStream inputStream = service.request().buildGet().invoke(InputStream.class);
            packageFeed = npmJacksonMapper.readValue(inputStream, PackageFeed.class);

            logger.debug("Downloaded NPM changes feed for [{}].", remoteRepository.getUrl());

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
            npmPackageFeedParser.parseFeed(repository, packageFeed);
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
    public class SearchPackagesEventListener
    {

        private NpmSearchRequest npmSearchRequest;

        public NpmSearchRequest getNpmSearchRequest()
        {
            return npmSearchRequest;
        }

        public void setNpmSearchRequest(NpmSearchRequest npmSearchRequest)
        {
            this.npmSearchRequest = npmSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event)
        {
            if (npmSearchRequest == null)
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

            logger.debug("NPM remote repository [{}] cached package existance is [{}]",
                         repository.getId(), packageExists);

            Runnable job = () -> fetchRemoteSearchResult(storageId, repositoryId, npmSearchRequest.getText(),
                                                         npmSearchRequest.getSize());
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
    public class ViewPackageEventListener
    {

        private NpmViewRequest npmSearchRequest;

        public NpmViewRequest getNpmSearchRequest()
        {
            return npmSearchRequest;
        }

        public void setNpmSearchRequest(NpmViewRequest npmSearchRequest)
        {
            this.npmSearchRequest = npmSearchRequest;
        }

        @EventListener
        public void handle(RemoteRepositorySearchEvent event)
        {
            if (npmSearchRequest == null)
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

            logger.debug("NPM remote repository [{}] cached package ixistance is [{}]",
                         repository.getId(), packagesExists);

            Runnable job = () -> fetchRemotePackageFeed(storage.getId(), repository.getId(),
                                                        npmSearchRequest.getPackageId());
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
        return artifactIdGroupRepository.artifactsExists(Collections.singleton(storageId + ":" + repositoryId),
                                                         predicate.getArtifactId(),
                                                         predicate.getCoordinateValues());
    }

    protected Configuration getConfiguration()
    {
        return configurationManager.getConfiguration();
    }

}
