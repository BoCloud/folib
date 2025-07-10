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
package com.folib.storage.repository.remote.heartbeat.monitor;

import com.google.common.collect.Lists;
import com.folib.components.DistributedCacheComponent;
import com.folib.constant.GlobalConstants;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.glassfish.jersey.client.ClientProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import jakarta.inject.Inject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Component
class HttpGetRemoteRepositoryCheckStrategy
        implements RemoteRepositoryHeartbeatMonitorStrategy {

    private static final Logger logger = LoggerFactory.getLogger(HttpGetRemoteRepositoryCheckStrategy.class);

    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Override
    public boolean isAlive(String storageId, String repositoryId, String remoteRepositoryUrl) {
        boolean flag;
        long startTime = System.currentTimeMillis();
        try {
            List<Integer> allowAccessList = getAllowAccessList();
            Client client = proxyRepositoryConnectionPoolConfigurationService.getRestClient(storageId, repositoryId);
            //连接建立超时时间
            client.property(ClientProperties.CONNECT_TIMEOUT, getConnectTimeout());
            //读取内容超时时间
            client.property(ClientProperties.READ_TIMEOUT, getReadTimeout());
            WebTarget target = client.target(remoteRepositoryUrl);
            Invocation.Builder builder = target.request();
            Response response = builder.head();
            int statusCode = response.getStatus();
            logger.info("The remote storageId [{}] repository [{}] url [{}] allow access [{}] response status [{}]", storageId, repositoryId, remoteRepositoryUrl, allowAccessList.stream().map(String::valueOf).collect(Collectors.joining(",")), statusCode);
            flag = allowAccessList.contains(statusCode) || String.valueOf(statusCode).startsWith("2");
        } catch (Exception e) {
            logger.error("Problem executing HTTP GET request to storageId [{}] repository [{}] url {}", storageId, repositoryId, remoteRepositoryUrl, e);
            flag = false;
        }
        logger.info("The remote storageId [{}] repository [{}] url [{}] take time [{}] ms", storageId, repositoryId, remoteRepositoryUrl, System.currentTimeMillis() - startTime);
        return flag;
    }

    private List<Integer> getAllowAccessList() {
        List<Integer> allowAccessList = Lists.newArrayList(HttpStatus.SC_OK, HttpStatus.SC_MOVED_PERMANENTLY, HttpStatus.SC_MOVED_TEMPORARILY, HttpStatus.SC_UNAUTHORIZED, HttpStatus.SC_NOT_FOUND, HttpStatus.SC_BAD_REQUEST, HttpStatus.SC_FORBIDDEN);
        String key = "REMOTE_REPOSITORY_ALLOW_ACCESS_CODE";
        String values = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(values)) {
            Arrays.asList(values.split(",")).forEach(item -> {
                int statusCode = Integer.parseInt(item);
                if (!allowAccessList.contains(statusCode)) {
                    allowAccessList.add(statusCode);
                }
            });
        }
        return allowAccessList;
    }

    private Integer getConnectTimeout() {
        int connectTimeout = GlobalConstants.DEFAULT_CONTENT_TIME;
        String key = "REMOTE_REPOSITORY_CONNECT_TIMEOUT";
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(value)) {
            connectTimeout = Integer.parseInt(value);
        }
        return connectTimeout * 1000;
    }

    private Integer getReadTimeout() {
        int readTimeout = GlobalConstants.DEFAULT_READ_TIME;
        String key = "REMOTE_REPOSITORY_READ_TIMEOUT";
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(value)) {
            readTimeout = Integer.parseInt(value);
        }
        return readTimeout * 1000;
    }

}
