package com.veadan.folib.providers.repository.proxied;

import java.util.Objects;

import javax.inject.Inject;

import com.veadan.folib.configuration.ConfigurationManager;
import com.veadan.folib.client.RemoteRepositoryRetryArtifactDownloadConfiguration;
import com.veadan.folib.client.RestArtifactResolver;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.springframework.stereotype.Component;

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

    public RestArtifactResolver newInstance(RemoteRepository repository)
    {
        Objects.requireNonNull(repository);
        
        RemoteRepositoryRetryArtifactDownloadConfiguration configuration = configurationManager.getConfiguration()
                                                                                               .getRemoteRepositoriesConfiguration()
                                                                                               .getRemoteRepositoryRetryArtifactDownloadConfiguration();
        
        String username = repository.getUsername();
        String password = repository.getPassword();
        String url = repository.getUrl();
        
        final HttpAuthenticationFeature authenticationFeature = (username != null && password != null) ? HttpAuthenticationFeature.basic(username, password) : null;
                
        return new RestArtifactResolver(proxyRepositoryConnectionPoolConfigurationService.getRestClient(), url,
                                        configuration,
                                        authenticationFeature)
                                {
                        
                                    @Override
                                    public boolean isAlive()
                                    {
                                        return remoteRepositoryAlivenessCacheManager.isAlive(repository);
                                    }
                        
                                };
    }

}
