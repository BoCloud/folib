package com.veadan.folib.service.impl;

import com.veadan.folib.config.ClientConfig;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;

import javax.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.core.Response;

import org.junit.jupiter.api.Test;
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
public class ProxyRepositoryConnectionPoolConfigurationServiceImplIT
{

    @Configuration
    @Import({ClientConfig.class})
    public static class SpringConfig
    {
    }

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Test
    public void setMaxPoolSize()
    {
        proxyRepositoryConnectionPoolConfigurationService.setMaxTotal(10);
        assertThat(proxyRepositoryConnectionPoolConfigurationService.getTotalStats().getMax()).isEqualTo(10);
    }

    @Test
    public void setDefaultMaxPerRepository()
    {
        proxyRepositoryConnectionPoolConfigurationService.setDefaultMaxPerRepository(8);
        assertThat(proxyRepositoryConnectionPoolConfigurationService.getDefaultMaxPerRepository()).isEqualTo(8);
    }

    @Test
    public void setMaxPerRepository()
    {
        String repositoryUrl = "http://repo.spring.io/snapshot";
        proxyRepositoryConnectionPoolConfigurationService.setMaxPerRepository(repositoryUrl, 3);
        assertThat(proxyRepositoryConnectionPoolConfigurationService.getPoolStats(repositoryUrl).getMax()).isEqualTo(3);
    }

    // integration test, external call to repo
    @Test
    public void connectionsReleasedTest()
    {
        String repositoryUrl = "http://repo.spring.io/snapshot";
        for (int i = 0; i < 3; i++)
        {
            Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient();
            Response response = client.target(repositoryUrl).request().get();
            response.close();
            client.close();
        }

        // all connections should be released
        assertThat(proxyRepositoryConnectionPoolConfigurationService.getTotalStats().getLeased()).isEqualTo(0);
    }

    // integration test, external call to repo
    @Test
    public void connectionsLeakedTest()
    {
        String repositoryUrl = "http://repo.spring.io/snapshot";
        proxyRepositoryConnectionPoolConfigurationService.setMaxPerRepository(repositoryUrl, 3);
        for (int i = 0; i < 3; i++)
        {
            proxyRepositoryConnectionPoolConfigurationService.getRestClient().target(repositoryUrl).request().get();
        }

        // all connections should be leaked
        assertThat(proxyRepositoryConnectionPoolConfigurationService.getTotalStats().getLeased()).isEqualTo(3);
    }
}
