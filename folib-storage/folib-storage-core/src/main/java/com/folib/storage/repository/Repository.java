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
package com.folib.storage.repository;

import com.folib.configuration.ProxyConfiguration;
import com.folib.configuration.UnionRepositoryConfiguration;
import com.folib.storage.Storage;
import com.folib.storage.repository.remote.RemoteRepository;
import com.folib.yaml.repository.RepositoryConfiguration;

import java.util.Set;

public interface Repository {

    String getId();

    String getBasedir();

    String getPolicy();

    String getStorageProvider();

    String getLayout();

    String getSubLayout();

    boolean getEnableCustomLayout();

    String getCustomLayout();

    String getType();

    boolean isSecured();

    String getStatus();

    long getArtifactMaxSize();

    boolean isAllowsForceDeletion();

    boolean isAllowsDeployment();

    boolean isAllowsRedeployment();

    boolean isAllowsDeletion();

    boolean isAllowsDirectoryBrowsing();

    boolean isChecksumHeadersEnabled();

    Set<String> getGroupRepositories();

    String getGroupDefaultRepository();

    Set<String> getArtifactCoordinateValidators();

    Storage getStorage();

    boolean isHostedRepository();

    boolean isProxyRepository();

    boolean isGroupRepository();

    boolean isInService();

    boolean isAcceptsSnapshots();

    boolean isAcceptsReleases();

    RepositoryConfiguration getRepositoryConfiguration();

    String getStorageIdAndRepositoryId();

    boolean isType(String type);

    RemoteRepository getRemoteRepository();

    ProxyConfiguration getProxyConfig();

    Set<String> getVulnerabilityWhites();

    Set<String> getVulnerabilityBlacks();

    Integer getScope();

    boolean isAllowAnonymous();

    UnionRepositoryConfiguration getUnionRepositoryConfig();
    boolean isSyncEnabled();

    Boolean getHealthStatus();
}
