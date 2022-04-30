package com.veadan.folib.service.impl;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.concurrent.TimeUnit;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;

import org.apache.http.HttpHost;
import org.apache.http.conn.routing.HttpRoute;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.pool.PoolStats;

import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;

import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.logging.LoggingFeature;
import org.glassfish.jersey.logging.LoggingFeature.Verbosity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

/**
 * @author korest
 */
@Component
public class ProxyRepositoryConnectionPoolConfigurationServiceImpl
        implements ProxyRepositoryConnectionPoolConfigurationService
{

    private static final Logger logger = LoggerFactory.getLogger(
            ProxyRepositoryConnectionPoolConfigurationServiceImpl.class);

    private PoolingHttpClientConnectionManager poolingHttpClientConnectionManager;
    private IdleConnectionMonitorThread idleConnectionMonitorThread;

    @Value("${pool.maxConnections:200}")
    private int maxTotal;
    @Value("${pool.defaultConnectionsPerRoute:5}")
    private int defaultMaxPerRoute;
    @Value("${pool.idleConnectionsTimeoutInSeconds:60}")
    private int idleConnectionsTimeoutInSeconds;

    @PostConstruct
    public void init()
    {
        poolingHttpClientConnectionManager = new PoolingHttpClientConnectionManager();
        poolingHttpClientConnectionManager.setMaxTotal(maxTotal); //TODO value that depends on number of threads?
        poolingHttpClientConnectionManager.setDefaultMaxPerRoute(defaultMaxPerRoute);

        // thread for monitoring unused connections
        idleConnectionMonitorThread =
                new IdleConnectionMonitorThread(poolingHttpClientConnectionManager, idleConnectionsTimeoutInSeconds);
        idleConnectionMonitorThread.setDaemon(true);
        idleConnectionMonitorThread.start();
    }

    @PreDestroy
    public void destroy()
    {
        shutdown();
    }

    @Override
    public Client getRestClient()
    {
        ClientConfig config = new ClientConfig();
        config.connectorProvider(new ApacheConnectorProvider());
        config.property(ApacheClientProperties.CONNECTION_MANAGER, poolingHttpClientConnectionManager);
        // property to prevent closing connection manager when client is closed
        config.property(ApacheClientProperties.CONNECTION_MANAGER_SHARED, true);

        java.util.logging.Logger logger = java.util.logging.Logger.getLogger("com.veadan.folib.RestClient");

        // TODO set basic authentication here instead of setting it always in client?
        /* CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));
        config.property(ApacheClientProperties.CREDENTIALS_PROVIDER, credentialsProvider); */

        return ClientBuilder.newBuilder()
                            .register(new LoggingFeature(logger, Verbosity.PAYLOAD_TEXT))
                            .withConfig(config)
                            .build();
    }

    @Override
    public CloseableHttpClient getHttpClient()
    {
        return HttpClients.custom()
                          .setConnectionManagerShared(true)
                          .setConnectionManager(poolingHttpClientConnectionManager)
                          .build();
    }

    @Override
    public void setMaxTotal(int max)
    {
        poolingHttpClientConnectionManager.setMaxTotal(max);
    }

    @Override
    public int getDefaultMaxPerRepository()
    {
        return poolingHttpClientConnectionManager.getDefaultMaxPerRoute();
    }

    @Override
    public void setDefaultMaxPerRepository(int defaultMax)
    {
        poolingHttpClientConnectionManager.setDefaultMaxPerRoute(defaultMax);
    }

    @Override
    public void setMaxPerRepository(String repository,
                                    int max)
    {
        if (max > 0)
        {
            HttpRoute httpRoute = getHttpRouteFromRepository(repository);
            poolingHttpClientConnectionManager.setMaxPerRoute(httpRoute, max);
        }
        else
        {
            logger.warn("Not setting max repository connections to {} as it is no positive value", max);
        }
    }

    @Override
    public PoolStats getTotalStats()
    {
        return poolingHttpClientConnectionManager.getTotalStats();
    }

    @Override
    public PoolStats getPoolStats(String repository)
    {
        HttpRoute httpRoute = getHttpRouteFromRepository(repository);
        return poolingHttpClientConnectionManager.getStats(httpRoute);
    }

    @Override
    public void shutdown()
    {
        idleConnectionMonitorThread.shutdown();
        poolingHttpClientConnectionManager.shutdown();
    }

    // code to create HttpRoute the same as in apache library
    private HttpRoute getHttpRouteFromRepository(String repository)
    {
        try
        {
            URI uri = new URI(repository);
            boolean secure = uri.getScheme().equalsIgnoreCase("https");
            int port = uri.getPort();
            if (uri.getPort() > 0)
            {
                port = uri.getPort();
            }
            else if (uri.getScheme().equalsIgnoreCase("https"))
            {
                port = 443;
            }
            else if (uri.getScheme().equalsIgnoreCase("http"))
            {
                port = 80;
            }
            else
            {
                logger.warn("Unknown port of uri {}", repository);
            }

            HttpHost httpHost = new HttpHost(uri.getHost(), port, uri.getScheme());
            // TODO check whether we need second param InetAddress
            return new HttpRoute(httpHost, null, secure);
        }
        catch (URISyntaxException e)
        {
            logger.error(e.getMessage(), e);
        }

        // default http route creation
        return new HttpRoute(HttpHost.create(repository));
    }

    private static final class IdleConnectionMonitorThread
            extends Thread
    {

        private PoolingHttpClientConnectionManager poolingHttpClientConnectionManager;

        private volatile boolean shutdown;

        private int idleConnectionsTimeout;

        IdleConnectionMonitorThread(PoolingHttpClientConnectionManager poolingHttpClientConnectionManager,
                                    int idleConnectionsTimeout)
        {
            super();
            this.poolingHttpClientConnectionManager = poolingHttpClientConnectionManager;
            this.idleConnectionsTimeout = idleConnectionsTimeout;
        }

        @Override
        public void run()
        {
            try
            {
                while (!shutdown)
                {
                    synchronized (this)
                    {
                        wait(5000);
                        poolingHttpClientConnectionManager.closeExpiredConnections();
                        poolingHttpClientConnectionManager.closeIdleConnections(idleConnectionsTimeout,
                                                                                TimeUnit.SECONDS);
                    }
                }
            }
            catch (InterruptedException e)
            {
                shutdown();
            }
        }

        public void shutdown()
        {
            shutdown = true;
            synchronized (this)
            {
                notifyAll();
            }
        }

    }
}
