package com.veadan.folib.services.impl;

import com.veadan.folib.configuration.ProxyConfiguration;
import com.veadan.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.veadan.folib.services.ConfigurationManagementService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpHost;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.routing.HttpRoute;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.NoopHostnameVerifier;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.pool.PoolStats;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;
import org.glassfish.jersey.logging.LoggingFeature;
import org.glassfish.jersey.logging.LoggingFeature.Verbosity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import java.net.URI;
import java.net.URISyntaxException;
import java.security.cert.X509Certificate;
import java.util.concurrent.TimeUnit;

/**
 * @author veadan
 */
@Slf4j
@Component
public class ProxyRepositoryConnectionPoolConfigurationServiceImpl
        implements ProxyRepositoryConnectionPoolConfigurationService {

    private static final Logger logger = LoggerFactory.getLogger(
            ProxyRepositoryConnectionPoolConfigurationServiceImpl.class);

    private PoolingHttpClientConnectionManager poolingHttpClientConnectionManager;
    private IdleConnectionMonitorThread idleConnectionMonitorThread;

    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Value("${pool.maxConnections:200}")
    private int maxTotal;
    @Value("${pool.defaultConnectionsPerRoute:5}")
    private int defaultMaxPerRoute;
    @Value("${pool.idleConnectionsTimeoutInSeconds:60}")
    private int idleConnectionsTimeoutInSeconds;

    @PostConstruct
    public void init() {
        poolingHttpClientConnectionManager = new PoolingHttpClientConnectionManager(getConnectionSocketFactory());
        //TODO value that depends on number of threads?
        poolingHttpClientConnectionManager.setMaxTotal(maxTotal);
        poolingHttpClientConnectionManager.setDefaultMaxPerRoute(defaultMaxPerRoute);

        // thread for monitoring unused connections
        idleConnectionMonitorThread =
                new IdleConnectionMonitorThread(poolingHttpClientConnectionManager, idleConnectionsTimeoutInSeconds);
        idleConnectionMonitorThread.setDaemon(true);
        idleConnectionMonitorThread.start();
    }

    @PreDestroy
    public void destroy() {
        shutdown();
    }


    private SSLContext getSslContext() {
        try {
            SSLContext sslcontext = SSLContext.getInstance("TLS");
            sslcontext.init(null, new TrustManager[]{new X509TrustManager() {
                @Override
                public void checkClientTrusted(X509Certificate[] arg0, String arg1) {
                }

                @Override
                public void checkServerTrusted(X509Certificate[] arg0, String arg1) {
                }

                @Override
                public X509Certificate[] getAcceptedIssuers() {
                    return new X509Certificate[0];
                }
            }}, new java.security.SecureRandom());
            return sslcontext;
        } catch (Exception e) {
            throw new RuntimeException("Failed to create SSL context", e);
        }
    }

    private Registry<ConnectionSocketFactory> getConnectionSocketFactory() {
        SSLConnectionSocketFactory socketFactory = new SSLConnectionSocketFactory(getSslContext(), NoopHostnameVerifier.INSTANCE);
        Registry<ConnectionSocketFactory> socketFactoryRegistry = RegistryBuilder.<ConnectionSocketFactory>create()
                .register("http", PlainConnectionSocketFactory.getSocketFactory())
                .register("https", socketFactory)
                .build();
        return socketFactoryRegistry;
    }

    @Override
    public Client getRestClient() {
        ClientConfig config = new ClientConfig();
        config.connectorProvider(new ApacheConnectorProvider());

        //全局代理配置
        ProxyConfiguration globalProxyConfig = configurationManagementService.getConfiguration().
                getProxyConfiguration();
        isExistProxy(globalProxyConfig, null, config);
        config.property(ApacheClientProperties.CONNECTION_MANAGER, poolingHttpClientConnectionManager);
        config.property(ApacheClientProperties.CONNECTION_MANAGER_SHARED, true);
        java.util.logging.Logger log = java.util.logging.Logger.getLogger("com.veadan.folib.RestClient");
        ClientBuilder clientBuilder = ClientBuilder.newBuilder()
                .register(new LoggingFeature(log, Verbosity.PAYLOAD_TEXT))
                .withConfig(config);
        return clientBuilder.build();
    }

    @Override
    public Client getRestClient(String storageId, String repositoryId) {
        logger.info("get rest client storageId [{}] repositoryId [{}]", storageId, repositoryId);
        ClientConfig config = new ClientConfig();

        //全局代理
        ProxyConfiguration globalProxyConfig = configurationManagementService.getConfiguration().
                getProxyConfiguration();

        //仓库代理
        ProxyConfiguration repositoryProxyConfig = configurationManagementService.
                getConfiguration().getRepository(storageId, repositoryId).getProxyConfig();
        config.connectorProvider(new ApacheConnectorProvider());
        isExistProxy(globalProxyConfig, repositoryProxyConfig, config);
        config.property(ApacheClientProperties.CONNECTION_MANAGER, poolingHttpClientConnectionManager);

        // property to prevent closing connection manager when client is closed
        config.property(ApacheClientProperties.CONNECTION_MANAGER_SHARED, true);

        java.util.logging.Logger log = java.util.logging.Logger.getLogger("com.veadan.folib.RestClient");

        // TODO set basic authentication here instead of setting it always in client?
        /* CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));
        config.property(ApacheClientProperties.CREDENTIALS_PROVIDER, credentialsProvider); */

        ClientBuilder clientBuilder = ClientBuilder.newBuilder()
                .register(new LoggingFeature(log, Verbosity.PAYLOAD_TEXT))
                .withConfig(config);
        return clientBuilder.build();
    }

    private void isExistProxy(ProxyConfiguration globalProxyConfig, ProxyConfiguration repositoryProxyConfig, ClientConfig config) {
        if (null != repositoryProxyConfig) {
            handleRepositoryProxy(globalProxyConfig, repositoryProxyConfig, config);
        } else {
            handleGlobalProxy(globalProxyConfig, config);
        }
    }

    private static String getProxyType(String type) {
        if (StringUtils.isBlank(type)) {
            type = "http";
        }
        return type.toLowerCase() + "://";
    }

    private static void handleRepositoryProxy(ProxyConfiguration globalProxyConfig,
                                              ProxyConfiguration repositoryProxyConfig, ClientConfig config) {
        String proxyType = "";
        if (StringUtils.isNotBlank(repositoryProxyConfig.getHost()) && null != repositoryProxyConfig.getPort()) {
            proxyType = getProxyType(repositoryProxyConfig.getType());
            config.property(ClientProperties.PROXY_URI,
                    proxyType + repositoryProxyConfig.getHost() + ":" + repositoryProxyConfig.getPort());
            config.property(ClientProperties.PROXY_USERNAME, repositoryProxyConfig.getUsername());
            config.property(ClientProperties.PROXY_PASSWORD, repositoryProxyConfig.getPassword());
            logger.info("get repository proxy config type [{}] host [{}] port [{}] username [{}]",
                    proxyType, repositoryProxyConfig.getHost(), repositoryProxyConfig.getPort(), repositoryProxyConfig.getUsername());
        } else {
            handleGlobalProxy(globalProxyConfig, config);
        }
    }

    private static void handleGlobalProxy(ProxyConfiguration globalProxyConfig, ClientConfig config) {
        String proxyType = "";
        if (null != globalProxyConfig) {
            if (StringUtils.isNotBlank(globalProxyConfig.getHost()) && null != globalProxyConfig.getPort()) {
                proxyType = getProxyType(globalProxyConfig.getType());
                config.property(ClientProperties.PROXY_URI,
                        proxyType + globalProxyConfig.getHost() + ":" + globalProxyConfig.getPort());
                config.property(ClientProperties.PROXY_USERNAME, globalProxyConfig.getUsername());
                config.property(ClientProperties.PROXY_PASSWORD, globalProxyConfig.getPassword());
                logger.info("get global proxy config type [{}] host [{}] port [{}] username [{}]",
                        proxyType, globalProxyConfig.getHost(), globalProxyConfig.getPort(), globalProxyConfig.getUsername());
            }
        }
    }

    @Override
    public CloseableHttpClient getHttpClient() {
        return HttpClients.custom()
                .setConnectionManagerShared(true)
                .setConnectionManager(poolingHttpClientConnectionManager)
                .build();
    }

    @Override
    public void setMaxTotal(int max) {
        poolingHttpClientConnectionManager.setMaxTotal(max);
    }

    @Override
    public int getDefaultMaxPerRepository() {
        return poolingHttpClientConnectionManager.getDefaultMaxPerRoute();
    }

    @Override
    public void setDefaultMaxPerRepository(int defaultMax) {
        poolingHttpClientConnectionManager.setDefaultMaxPerRoute(defaultMax);
    }

    @Override
    public void setMaxPerRepository(String repository,
                                    int max) {
        if (max > 0) {
            HttpRoute httpRoute = getHttpRouteFromRepository(repository);
            poolingHttpClientConnectionManager.setMaxPerRoute(httpRoute, max);
        } else {
            logger.warn("Not setting max repository connections to {} as it is no positive value", max);
        }
    }

    @Override
    public PoolStats getTotalStats() {
        return poolingHttpClientConnectionManager.getTotalStats();
    }

    @Override
    public PoolStats getPoolStats(String repository) {
        HttpRoute httpRoute = getHttpRouteFromRepository(repository);
        return poolingHttpClientConnectionManager.getStats(httpRoute);
    }

    @Override
    public void shutdown() {
        idleConnectionMonitorThread.shutdown();
        poolingHttpClientConnectionManager.shutdown();
    }

    // code to create HttpRoute the same as in apache library
    private HttpRoute getHttpRouteFromRepository(String repository) {
        try {
            URI uri = new URI(repository);
            boolean secure = uri.getScheme().equalsIgnoreCase("https");
            int port = uri.getPort();
            if (uri.getPort() > 0) {
                port = uri.getPort();
            } else if (uri.getScheme().equalsIgnoreCase("https")) {
                port = 443;
            } else if (uri.getScheme().equalsIgnoreCase("http")) {
                port = 80;
            } else {
                logger.warn("Unknown port of uri {}", repository);
            }

            HttpHost httpHost = new HttpHost(uri.getHost(), port, uri.getScheme());
            // TODO check whether we need second param InetAddress
            return new HttpRoute(httpHost, null, secure);
        } catch (URISyntaxException e) {
            logger.error(e.getMessage(), e);
        }

        // default http route creation
        return new HttpRoute(HttpHost.create(repository));
    }

    private static final class IdleConnectionMonitorThread
            extends Thread {

        private PoolingHttpClientConnectionManager poolingHttpClientConnectionManager;

        private volatile boolean shutdown;

        private int idleConnectionsTimeout;

        IdleConnectionMonitorThread(PoolingHttpClientConnectionManager poolingHttpClientConnectionManager,
                                    int idleConnectionsTimeout) {
            super();
            this.poolingHttpClientConnectionManager = poolingHttpClientConnectionManager;
            this.idleConnectionsTimeout = idleConnectionsTimeout;
        }

        @Override
        public void run() {
            try {
                while (!shutdown) {
                    synchronized (this) {
                        wait(5000);
                        poolingHttpClientConnectionManager.closeExpiredConnections();
                        poolingHttpClientConnectionManager.closeIdleConnections(idleConnectionsTimeout,
                                TimeUnit.SECONDS);
                    }
                }
            } catch (InterruptedException e) {
                shutdown();
            }
        }

        public void shutdown() {
            shutdown = true;
            synchronized (this) {
                notifyAll();
            }
        }

    }
}
