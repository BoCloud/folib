/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.services.impl;

import com.alibaba.fastjson.JSONObject;
import com.google.common.collect.Lists;
import com.folib.components.DistributedCacheComponent;
import com.folib.configuration.MutableProxyConfiguration;
import com.folib.configuration.ProxyConfiguration;
import com.folib.enums.ProductTypeEnum;
import com.folib.ext.jersey.ContentTypeFixerFilter;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.remote.RemoteRepository;
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
import org.springframework.context.annotation.Lazy;
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
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
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

    @Lazy
    @Autowired
    private ConfigurationManagementService configurationManagementService;

    @Autowired
    private DistributedCacheComponent distributedCacheComponent;

    @Value("${pool.maxConnections:500}")
    private int maxTotal;
    @Value("${pool.defaultConnectionsPerRoute:10}")
    private int defaultMaxPerRoute;
    @Value("${pool.idleConnectionsTimeoutInSeconds:600}")
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
        java.util.logging.Logger log = java.util.logging.Logger.getLogger("com.folib.RestClient");
        ClientBuilder clientBuilder = ClientBuilder.newBuilder()
                .register(new LoggingFeature(log, Verbosity.PAYLOAD_TEXT))
                .withConfig(config);
        return clientBuilder.build().register(new ContentTypeFixerFilter());
    }

    @Override
    public Client getRestClient(String storageId, String repositoryId) {
        logger.debug("Get rest client storageId [{}] repositoryId [{}]", storageId, repositoryId);
        ClientConfig config = new ClientConfig();

        //全局代理
        ProxyConfiguration globalProxyConfig = configurationManagementService.getConfiguration().
                getProxyConfiguration();

        //仓库代理
        Repository repository = configurationManagementService.
                getConfiguration().getRepository(storageId, repositoryId);
        ProxyConfiguration repositoryProxyConfig = repository.getProxyConfig();
        config.connectorProvider(new ApacheConnectorProvider());
        isExistProxy(globalProxyConfig, repositoryProxyConfig, config);
        config.property(ApacheClientProperties.CONNECTION_MANAGER, poolingHttpClientConnectionManager);

        // property to prevent closing connection manager when client is closed
        config.property(ApacheClientProperties.CONNECTION_MANAGER_SHARED, true);
        dockerRegistry(repository, config);

        java.util.logging.Logger log = java.util.logging.Logger.getLogger("com.folib.RestClient");

        // TODO set basic authentication here instead of setting it always in client?
        /* CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password));
        config.property(ApacheClientProperties.CREDENTIALS_PROVIDER, credentialsProvider); */

        ClientBuilder clientBuilder = ClientBuilder.newBuilder()
                .register(new LoggingFeature(log, Verbosity.PAYLOAD_TEXT))
                .withConfig(config);
        return clientBuilder.build().register(new ContentTypeFixerFilter());
    }

    @Override
    public Client getRestClient(String repositoryProxyConfigParam) {
        ClientConfig config = new ClientConfig();
        //全局代理
        ProxyConfiguration globalProxyConfig = configurationManagementService.getConfiguration().
                getProxyConfiguration();
        ProxyConfiguration repositoryProxyConfig = null;
        if (StringUtils.isNotBlank(repositoryProxyConfigParam)) {
            //仓库代理
            repositoryProxyConfig = new ProxyConfiguration(JSONObject.parseObject(repositoryProxyConfigParam, MutableProxyConfiguration.class));
        }
        config.connectorProvider(new ApacheConnectorProvider());
        isExistProxy(globalProxyConfig, repositoryProxyConfig, config);
        config.property(ApacheClientProperties.CONNECTION_MANAGER, poolingHttpClientConnectionManager);
        // property to prevent closing connection manager when client is closed
        config.property(ApacheClientProperties.CONNECTION_MANAGER_SHARED, true);

        java.util.logging.Logger log = java.util.logging.Logger.getLogger("com.folib.RestClient");

        ClientBuilder clientBuilder = ClientBuilder.newBuilder()
                .register(new LoggingFeature(log, Verbosity.PAYLOAD_TEXT))
                .withConfig(config);
        return clientBuilder.build().register(new ContentTypeFixerFilter());
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
            logger.debug("Get repository proxy config type [{}] host [{}] port [{}] username [{}]",
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
                logger.debug("Get global proxy config type [{}] host [{}] port [{}] username [{}]",
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

    private void dockerRegistry(Repository repository, ClientConfig config) {
        if (!ProductTypeEnum.Docker.getFoLibraryName().equalsIgnoreCase(repository.getSubLayout())) {
            return;
        }
        RemoteRepository remoteRepository = repository.getRemoteRepository();
        if (Objects.isNull(remoteRepository)) {
            return;
        }
        String url = remoteRepository.getUrl();
        if (StringUtils.isBlank(url)) {
            return;
        }
        if (getDisableDomainList().stream().anyMatch(url::startsWith)) {
            //禁用自动重定向
            config.property(ClientProperties.FOLLOW_REDIRECTS, false);
        }
    }

    private List<String> getDisableDomainList() {
        List<String> disableDomainList = Lists.newArrayList("https://registry-1.docker.io", "https://docker.m.daocloud.io");
        String key = "REMOTE_REPOSITORY_DISABLE_FOLLOW_REDIRECTS_DOMAIN";
        String values = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(values)) {
            Arrays.asList(values.split(",")).forEach(item -> {
                if (StringUtils.isNotBlank(item) && !disableDomainList.contains(item)) {
                    disableDomainList.add(item);
                }
            });
        }
        logger.info("Docker disable follow redirects domain [{}]", String.join(",", disableDomainList));
        return disableDomainList;
    }
}
