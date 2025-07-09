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
package com.folib.converters.configuration;

import com.folib.storage.repository.RepositoryDto;
import com.folib.forms.configuration.RepositoryForm;
import com.folib.storage.repository.MutableHttpConnectionPool;

import org.springframework.core.convert.converter.Converter;

import java.util.Objects;

/**
 * @author veadan
 */
public enum RepositoryFormConverter
        implements Converter<RepositoryForm, RepositoryDto>
{
    INSTANCE;

    @Override
    public RepositoryDto convert(final RepositoryForm source)
    {
        RepositoryDto result = new RepositoryDto();
        result.setId(source.getId());
        result.setPolicy(source.getPolicy());
        result.setStorageProvider(source.getStorageProvider());
        result.setLayout(source.getLayout());
        result.setSubLayout(source.getSubLayout());
        result.setEnableCustomLayout(source.getEnableCustomLayout());
        result.setCustomLayout(source.getCustomLayout());
        result.setType(source.getType());
        result.setSecured(source.isSecured());
        result.setStatus(source.getStatus());
        result.setArtifactMaxSize(source.getArtifactMaxSize());
        result.setAllowsForceDeletion(source.isAllowsForceDeletion());
        result.setAllowsDeployment(source.isAllowsDeployment());
        result.setAllowsRedeployment(source.isAllowsRedeployment());
        result.setAllowsDeletion(source.isAllowsDeletion());
        result.setAllowsDirectoryBrowsing(source.isAllowsDirectoryBrowsing());
        result.setChecksumHeadersEnabled(source.isChecksumHeadersEnabled());
        result.setGroupDefaultRepository(source.getGroupDefaultRepository());
        if (source.getRepositoryConfiguration() != null)
        {
            result.setRepositoryConfiguration(
                    source.getRepositoryConfiguration().accept(CustomRepositoryConfigurationFormConverter.INSTANCE));
        }
        if (source.getProxyConfiguration() != null)
        {
            result.setProxyConfiguration(ProxyConfigurationFormConverter.INSTANCE.convert(
                    source.getProxyConfiguration()));
        }
        if (source.getRemoteRepository() != null)
        {
            result.setRemoteRepository(
                    RemoteRepositoryFormConverter.INSTANCE.convert(source.getRemoteRepository()));
        }
        if (source.getHttpConnectionPool() != null)
        {
            MutableHttpConnectionPool httpConnectionPool = new MutableHttpConnectionPool();
            httpConnectionPool.setAllocatedConnections(source.getHttpConnectionPool());
            result.setHttpConnectionPool(httpConnectionPool);
        }
        if (source.getGroupRepositories() != null)
        {
            result.setGroupRepositories(source.getGroupRepositories());
        }
        if (source.getArtifactCoordinateValidators() != null)
        {
            result.setArtifactCoordinateValidators(source.getArtifactCoordinateValidators());
        }
        result.setBasedir(source.getBasedir());
        if (Objects.nonNull(source.getScope())) {
            result.setScope(source.getScope());
        }
        result.setAllowAnonymous(source.isAllowAnonymous());
        return result;
    }
}
