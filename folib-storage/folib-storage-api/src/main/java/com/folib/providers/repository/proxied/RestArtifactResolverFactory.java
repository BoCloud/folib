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
package com.folib.providers.repository.proxied;

import com.folib.client.RemoteRepositoryRetryArtifactDownloadConfiguration;
import com.folib.client.RestArtifactResolver;
import com.folib.config.CustomAuthenticationFeature;
import com.folib.configuration.ConfigurationManager;
import com.folib.providers.io.RepositoryPath;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.storage.repository.remote.heartbeat.RemoteRepositoryAlivenessService;
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
