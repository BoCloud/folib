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
package com.folib.forms.configuration;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.folib.storage.repository.RepositoryPolicyEnum;
import com.folib.storage.repository.RepositoryStatusEnum;
import com.folib.storage.repository.RepositoryTypeEnum;
import com.folib.validation.configuration.DescribableEnumValue;
import com.folib.validation.configuration.LayoutProviderValue;
import com.folib.validation.configuration.StorageProviderValue;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.PositiveOrZero;
import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author veadan
 */
public class RepositoryForm {

    @NotEmpty(message = "An id must be specified.")
    @Pattern(regexp = "[a-zA-Z0-9\\-\\_\\.]+")
    private String id;

    private String basedir;

    @NotEmpty(message = "A policy must be specified.")
    @DescribableEnumValue(message = "The policy value is invalid.", type = RepositoryPolicyEnum.class)
    private String policy;

    @NotEmpty(message = "A storage provider must be specified.")
    @StorageProviderValue(message = "The storage provider value is invalid.")
    private String storageProvider;

    @NotEmpty(message = "A layout must be specified.")
    @LayoutProviderValue(message = "The layout value is invalid.")
    private String layout;

    private String subLayout;

    /**
     * 是否启用自定义布局
     */
    private boolean enableCustomLayout;

    /**
     * 自定义布局
     */
    private String customLayout;

    @NotEmpty(message = "A type must be specified.")
    @DescribableEnumValue(message = "The type value is invalid.", type = RepositoryTypeEnum.class)
    private String type;

    private boolean secured;

    @NotEmpty(message = "A status must be specified.")
    @DescribableEnumValue(message = "The status value is invalid.", type = RepositoryStatusEnum.class)
    private String status;

    private String storageId;

    private long artifactMaxSize;

    private boolean allowsForceDeletion;

    private boolean allowsDeployment = true;

    private boolean allowsRedeployment;

    private boolean allowsDeletion = true;

    private boolean allowsDirectoryBrowsing = true;

    private boolean checksumHeadersEnabled;

    @Valid
    private ProxyConfigurationForm proxyConfiguration;

    @Valid
    private RemoteRepositoryForm remoteRepository;

    @PositiveOrZero(message = "The httpConnectionPool value must be greater, or equal to zero.")
    private Integer httpConnectionPool;

    @Valid
    private CustomRepositoryConfigurationForm repositoryConfiguration;

    private Set<String> groupRepositories;

    public String getGroupDefaultRepository() {
        return groupDefaultRepository;
    }

    public void setGroupDefaultRepository(String groupDefaultRepository) {
        this.groupDefaultRepository = groupDefaultRepository;
    }

    private String groupDefaultRepository;

    private Set<String> artifactCoordinateValidators;
    /**
     * 白名单列表
     */
    @NotEmpty(message = "请填写白名单", groups = {WhiteGroup.class})
    private Set<String> vulnerabilityWhites;
    /**
     * 黑名单列表
     */
    @NotEmpty(message = "请填写黑名单", groups = {BlackGroup.class})
    private Set<String> vulnerabilityBlacks;

    public boolean isSyncEnabled() {
        return syncEnabled;
    }

    public void setSyncEnabled(boolean syncEnabled) {
        this.syncEnabled = syncEnabled;
    }

    /**是否同步存储空间到其他节点*/
    private boolean syncEnabled;
    /**
     * 仓库可见范围 1 存储空间内 2 公开
     */
    private Integer scope = 1;

    private boolean allowAnonymous = true;

    public String getId() {
        return id;
    }

    public void setId(final String id) {
        this.id = id;
    }

    public String getBasedir() {
        return basedir;
    }

    public void setBasedir(final String basedir) {
        this.basedir = basedir;
    }

    public String getPolicy() {
        return policy;
    }

    public void setPolicy(final String policy) {
        this.policy = policy;
    }

    public String getStorageProvider() {
        return storageProvider;
    }

    public void setStorageProvider(final String storageProvider) {
        this.storageProvider = storageProvider;
    }

    public String getLayout() {
        return layout;
    }

    public void setLayout(final String layout) {
        this.layout = layout;
    }

    public boolean getEnableCustomLayout() {
        return enableCustomLayout;
    }

    public void setEnableCustomLayout(boolean enableCustomLayout) {
        this.enableCustomLayout = enableCustomLayout;
    }

    public String getCustomLayout() {
        return customLayout;
    }

    public void setCustomLayout(String customLayout) {
        this.customLayout = customLayout;
    }

    public String getType() {
        return type;
    }

    public void setType(final String type) {
        this.type = type;
    }

    public boolean isSecured() {
        return secured;
    }

    public void setSecured(final boolean secured) {
        this.secured = secured;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(final String status) {
        this.status = status;
    }

    public long getArtifactMaxSize() {
        return artifactMaxSize;
    }

    public void setArtifactMaxSize(final long artifactMaxSize) {
        this.artifactMaxSize = artifactMaxSize;
    }

    public boolean isAllowsForceDeletion() {
        return allowsForceDeletion;
    }

    public void setAllowsForceDeletion(final boolean allowsForceDeletion) {
        this.allowsForceDeletion = allowsForceDeletion;
    }

    public boolean isAllowsDeployment() {
        return allowsDeployment;
    }

    public void setAllowsDeployment(final boolean allowsDeployment) {
        this.allowsDeployment = allowsDeployment;
    }

    public boolean isAllowsRedeployment() {
        return allowsRedeployment;
    }

    public void setAllowsRedeployment(final boolean allowsRedeployment) {
        this.allowsRedeployment = allowsRedeployment;
    }

    public boolean isAllowsDeletion() {
        return allowsDeletion;
    }

    public void setAllowsDeletion(final boolean allowsDeletion) {
        this.allowsDeletion = allowsDeletion;
    }

    public boolean isAllowsDirectoryBrowsing() {
        return allowsDirectoryBrowsing;
    }

    public void setAllowsDirectoryBrowsing(final boolean allowsDirectoryBrowsing) {
        this.allowsDirectoryBrowsing = allowsDirectoryBrowsing;
    }

    public boolean isChecksumHeadersEnabled() {
        return checksumHeadersEnabled;
    }

    public void setChecksumHeadersEnabled(final boolean checksumHeadersEnabled) {
        this.checksumHeadersEnabled = checksumHeadersEnabled;
    }

    public ProxyConfigurationForm getProxyConfiguration() {
        return proxyConfiguration;
    }

    public void setProxyConfiguration(final ProxyConfigurationForm proxyConfiguration) {
        this.proxyConfiguration = proxyConfiguration;
    }

    public RemoteRepositoryForm getRemoteRepository() {
        return remoteRepository;
    }

    public void setRemoteRepository(final RemoteRepositoryForm remoteRepository) {
        this.remoteRepository = remoteRepository;
    }

    public Integer getHttpConnectionPool() {
        return httpConnectionPool;
    }

    public void setHttpConnectionPool(final Integer httpConnectionPool) {
        this.httpConnectionPool = httpConnectionPool;
    }

    public CustomRepositoryConfigurationForm getRepositoryConfiguration() {
        return repositoryConfiguration;
    }

    public void setRepositoryConfiguration(final CustomRepositoryConfigurationForm repositoryConfiguration) {
        this.repositoryConfiguration = repositoryConfiguration;
    }

    public Set<String> getGroupRepositories() {
        return groupRepositories;
    }

    @JsonDeserialize(as = LinkedHashSet.class)
    public void setGroupRepositories(final Set<String> groupRepositories) {
        this.groupRepositories = groupRepositories;
    }

    public Set<String> getArtifactCoordinateValidators() {
        return artifactCoordinateValidators;
    }

    public void setArtifactCoordinateValidators(final Set<String> artifactCoordinateValidators) {
        this.artifactCoordinateValidators = artifactCoordinateValidators;
    }

    public Set<String> getVulnerabilityWhites() {
        return vulnerabilityWhites;
    }

    public void setVulnerabilityWhites(Set<String> vulnerabilityWhites) {
        this.vulnerabilityWhites = vulnerabilityWhites;
    }

    public Set<String> getVulnerabilityBlacks() {
        return vulnerabilityBlacks;
    }

    public void setVulnerabilityBlacks(Set<String> vulnerabilityBlacks) {
        this.vulnerabilityBlacks = vulnerabilityBlacks;
    }

    public String getStorageId() {
        return storageId;
    }

    public void setStorageId(String storageId) {
        this.storageId = storageId;
    }

    public String getSubLayout() {
        return subLayout;
    }

    public void setSubLayout(String subLayout) {
        this.subLayout = subLayout;
    }

    public Integer getScope() {
        return scope;
    }

    public void setScope(Integer scope) {
        this.scope = scope;
    }

    public boolean isAllowAnonymous() {
        return allowAnonymous;
    }

    public void setAllowAnonymous(boolean allowAnonymous) {
        this.allowAnonymous = allowAnonymous;
    }

    public interface WhiteGroup
            extends Serializable {
        // 白名单组
    }

    public interface BlackGroup
            extends Serializable {
        // 白名单组
    }

}
