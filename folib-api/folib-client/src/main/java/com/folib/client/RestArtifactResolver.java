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
package com.folib.client;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Feature;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import java.io.Closeable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.glassfish.jersey.client.ClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

/**
 */
public class RestArtifactResolver
        implements Closeable
{

    private static final Logger logger = LoggerFactory.getLogger(RestArtifactResolver.class);

    private final String repositoryBaseUrl;
    private String targetUrl;
    private MultivaluedMap<String, Object> headers;
    private final Client client;
    private Feature authentication;
    private RemoteRepositoryRetryArtifactDownloadConfiguration configuration;

    private ClientRequestFilter bearerTokenAuthFilter;

    public RestArtifactResolver(Client client,
                                String repositoryBaseUrl,
                                String targetUrl,
                                MultivaluedMap<String, Object> headers,
                                RemoteRepositoryRetryArtifactDownloadConfiguration configuration,
                                ClientRequestFilter bearerTokenAuthFilter)
    {
        this.client = client;
        //连接建立超时时间
        Integer connectTimeOut = globalClientConnectTimeOut();
        if (Objects.nonNull(connectTimeOut)) {
            this.client.property(ClientProperties.CONNECT_TIMEOUT, connectTimeOut);
        }
        this.targetUrl = targetUrl;
        this.headers = headers;
        this.repositoryBaseUrl = normalize(repositoryBaseUrl);
        this.configuration = configuration;
        this.bearerTokenAuthFilter = bearerTokenAuthFilter;
    }

    public RestArtifactResolver(Client client,
                                String repositoryBaseUrl,
                                String targetUrl,
                                MultivaluedMap<String, Object> headers,
                                RemoteRepositoryRetryArtifactDownloadConfiguration configuration,
                                Feature authentication,
                                ClientRequestFilter bearerTokenAuthFilter )
    {
        this(client, repositoryBaseUrl, targetUrl, headers, configuration,bearerTokenAuthFilter);
        this.authentication = authentication;
    }

    public RemoteRepositoryRetryArtifactDownloadConfiguration getConfiguration()
    {
        return configuration;
    }

    public boolean isAlive()
    {
        return true;
    }

    @Override
    public void close()
    {
        if (client != null)
        {
            client.close();
        }
    }

    public CloseableRestResponse get(String path)
    {
        return get(path, 0);
    }

    public CloseableRestResponse get(String path,
                                     long offset)
    {
        String url = escapeUrl(path);
        if(StringUtils.hasText(targetUrl)){
            url = targetUrl;
        }
        logger.debug("Getting {}...", url);

        WebTarget resource = new WebTargetBuilder(url).withAuthentication()
                                                      .customRequestConfig()
                                                      .build();
        long startTime = System.currentTimeMillis();
        logger.debug("Url [{}] 开始于 [{}]", url, startTime);
        MultivaluedMap<String, Object> acceptHeaders = new MultivaluedHashMap();
        String userAgentKey = "User-Agent";
        List<Object> acceptList = new ArrayList<>();
        acceptList.add("FoLibrary/1.0.0");
        acceptHeaders.put(userAgentKey, acceptList);
        Invocation.Builder request = resource.request();
        if (Objects.isNull(headers)) {
            headers = acceptHeaders;
        } else if (!headers.containsKey(userAgentKey)){
            headers.add(userAgentKey, acceptList);
        }
        request.headers(headers);
        Response response;

        if (offset > 0)
        {
            response = request.header("Range", "bytes=" + offset + "-").get();
        }
        else
        {
            response = request.get();
        }
        long endTime = System.currentTimeMillis();
        logger.debug("Url [{}] 结束于 [{}] 耗时约 [{}] 毫秒", url, endTime, endTime - startTime);
        return new CloseableRestResponse(response);
    }

    public CloseableRestResponse head(String path)
    {
        String url = escapeUrl(path);

        logger.debug("Heading {}...", url);

        WebTarget resource = new WebTargetBuilder(url)
                                     .withAuthentication()
                                     .customRequestConfig()
                                     .build();

        return new CloseableRestResponse(resource.request().head());
    }

    private String escapeUrl(String path)
    {
        String baseUrl = repositoryBaseUrl + (repositoryBaseUrl.endsWith("/") ? "" : "/");
        String p = (path.startsWith("/") ? path.substring(1, path.length()) : path);

        return baseUrl + p;
    }

    private String normalize(String repositoryBaseUrl)
    {
        return repositoryBaseUrl.endsWith("/") ? repositoryBaseUrl : repositoryBaseUrl + "/";
    }

    private class WebTargetBuilder
    {

        private final WebTarget target;

        private WebTargetBuilder(String uri)
        {
            this.target = client.target(uri);
        }

        private WebTargetBuilder withAuthentication()
        {
            if (authentication != null)
            {
                target.register(authentication);
            }
            if (bearerTokenAuthFilter != null) {
                target.register(bearerTokenAuthFilter);
            }
            return this;
        }

        private WebTargetBuilder customRequestConfig()
        {
            target.property(ApacheClientProperties.REQUEST_CONFIG,
                            RequestConfig.custom().setCircularRedirectsAllowed(true).build());
            return this;
        }

        public WebTarget build()
        {
            return target;
        }
    }

    private static Integer globalClientConnectTimeOut() {
        String key = "globalClientConnectTimeOut";
        String globalClientConnectTimeOut = System.getProperty(key);
        if (!StringUtils.hasText(globalClientConnectTimeOut)) {
            return null;
        }
        return Integer.parseInt(globalClientConnectTimeOut);
    }

    public Client getClient() {
        return client;
    }

    public String getTargetUrl() {
        return targetUrl;
    }

    public void setTargetUrl(String targetUrl) {
        this.targetUrl = targetUrl;
    }

    public MultivaluedMap<String, Object> getHeaders() {
        return headers;
    }

    public void setHeaders(MultivaluedMap<String, Object> headers) {
        this.headers = headers;
    }
}
