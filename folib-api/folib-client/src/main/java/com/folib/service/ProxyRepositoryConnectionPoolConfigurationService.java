package com.folib.service;

import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.pool.PoolStats;

import javax.ws.rs.client.Client;

/**
 * @author veadan
 */
public interface ProxyRepositoryConnectionPoolConfigurationService
{

    Client getRestClient();

    Client getRestClient(String storageId,String repositoryId);

    Client getRestClient(String repositoryProxyConfigParam);

    CloseableHttpClient getHttpClient();

    void setMaxTotal(int max);

    void setDefaultMaxPerRepository(int defaultMax);

    int getDefaultMaxPerRepository();

    void setMaxPerRepository(String repository, int max);

    PoolStats getTotalStats();

    PoolStats getPoolStats(String repository);

    void shutdown();
}
