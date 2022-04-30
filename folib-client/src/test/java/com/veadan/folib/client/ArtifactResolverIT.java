package com.veadan.folib.client;

import com.veadan.folib.config.ClientConfig;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;

import javax.inject.Inject;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author korest
 */
@SpringBootTest
@ActiveProfiles(profiles = "test")
@ContextConfiguration
public class ArtifactResolverIT
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactResolverIT.class);

    @Configuration
    @Import({ClientConfig.class})
    public static class SpringConfig
    {
    }

    private RestArtifactResolver artifactResolver;

    // fake url
    private String repositoryUrl = "https://repo.maven.apache.org/maven2/";

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @BeforeEach
    public void setUp()
    {
        artifactResolver = new RestArtifactResolver(proxyRepositoryConnectionPoolConfigurationService.getRestClient(),
                                                    repositoryUrl,
                                                    new RemoteRepositoryRetryArtifactDownloadConfiguration(MutableRemoteRepositoryRetryArtifactDownloadConfiguration.DEFAULT));
    }

    @Test
    public void allConnectionsReleasedTest() throws InterruptedException
    {
        MultiHttpClientConnThread[] threads = new MultiHttpClientConnThread[10];
        for (int i = 0; i < threads.length; i++)
        {
            threads[i] = new MultiHttpClientConnThread(artifactResolver, repositoryUrl);
            threads[i].start();
        }

        for (int i = 0; i < threads.length; i++)
        {
            threads[i].join();
        }

        // all connections should be released
        assertThat(proxyRepositoryConnectionPoolConfigurationService.getPoolStats(repositoryUrl).getLeased()).isEqualTo(0);
    }

    public static final class MultiHttpClientConnThread extends Thread
    {

        private RestArtifactResolver artifactResolver;

        private String url;

        public MultiHttpClientConnThread(RestArtifactResolver artifactResolver,
                                         String url)
        {
            this.artifactResolver = artifactResolver;
            this.url = url;
        }

        @Override
        public final void run()
        {
            try (CloseableRestResponse response = artifactResolver.get(url))
            {
                // Left empty because the original method had no other logic than the now-removed
                // closure of a CloseableRestResponse
            }
            catch (Exception e)
            {
                logger.error(e.getMessage(), e);
            }
        }

    }
}
