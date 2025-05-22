package com.veadan.folib.providers.repository.proxied;

import com.veadan.folib.client.RemoteRepositoryRetryArtifactDownloadConfiguration;
import com.veadan.folib.client.RestArtifactResolver;
import com.veadan.folib.config.CustomAuthenticationFeature;
import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import javax.ws.rs.client.Client;
import java.util.Objects;

/**
 * @author veadan
 */
@Component
public class RestArtifactResolverFactory
{

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private ConfigurationManager configurationManager;

    @Inject
    private RemoteRepositoryAlivenessService remoteRepositoryAlivenessCacheManager;

    public RestArtifactResolver newInstance(RemoteRepository repository, RepositoryPath repositoryPath)
    {
        Objects.requireNonNull(repository);

        RemoteRepositoryRetryArtifactDownloadConfiguration configuration = configurationManager.getConfiguration()
                .getRemoteRepositoriesConfiguration()
                .getRemoteRepositoryRetryArtifactDownloadConfiguration();

        String username = repository.getUsername();
        String password = repository.getPassword();
        String url = repository.getUrl();

        final CustomAuthenticationFeature customAuthenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? CustomAuthenticationFeature.create(username, password) : null;
        final BearerTokenAuthFilter bearerTokenAuthFilter = (StringUtils.isEmpty(username) && StringUtils.isNotBlank(password))  ? new BearerTokenAuthFilter( password) : null;
        Client client  = proxyRepositoryConnectionPoolConfigurationService.getRestClient(repositoryPath.getStorageId(),repositoryPath.getRepositoryId());
        return new RestArtifactResolver(client , url, repositoryPath.getTargetUrl(), repositoryPath.getHeaders(),
                                        configuration,
                                        customAuthenticationFeature,
                                        bearerTokenAuthFilter)
                                {

            @Override
            public boolean isAlive()
            {
                return remoteRepositoryAlivenessCacheManager.isAlive(repository);
            }

        };
    }

}
