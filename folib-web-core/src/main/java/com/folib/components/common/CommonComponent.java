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
package com.folib.components.common;

import com.google.common.collect.Lists;
import com.folib.ldap.LdapAuthenticationConfigurationManager;
import com.folib.authorization.dto.Role;
import com.folib.authorization.service.AuthorizationConfigService;
import com.folib.components.DistributedCacheComponent;
import com.folib.config.CustomAuthenticationFeature;
import com.folib.configuration.AdvancedConfiguration;
import com.folib.constant.GlobalConstants;
import com.folib.job.cron.services.CronTaskConfigurationService;
import com.folib.enums.StorageProviderEnum;
import com.folib.forms.configuration.ServerSettingsForm;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.StorageManagementService;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryDto;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.security.AuthoritiesProvider;
import com.folib.users.userdetails.SpringSecurityUser;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.http.client.config.RequestConfig;
import org.glassfish.jersey.apache.connector.ApacheClientProperties;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.AnonymousAuthenticationFilter;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import javax.ws.rs.client.WebTarget;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * @author veadan
 **/
@Slf4j
@Component
public class CommonComponent {

    @Inject
    @Lazy
    private AuthorizationConfigService authorizationConfigService;

    @Inject
    @Lazy
    private ConfigurationManagementService configurationManagementService;

    @Inject
    @Lazy
    private LdapAuthenticationConfigurationManager ldapAuthenticationManager;

    @Inject
    @Lazy
    private StorageManagementService storageManagementService;

    @Inject
    private DistributedCacheComponent distributedCacheComponent;

    @Inject
    private AuthoritiesProvider authoritiesProvider;

    @Inject
    private AnonymousAuthenticationFilter anonymousAuthenticationFilter;

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    /**
     * Client WebTarget 构建认证信息
     *
     * @param webTarget webTarget
     * @param username  username
     * @param password  password
     */
    public void authentication(WebTarget webTarget, String username, String password) {
        final CustomAuthenticationFeature customAuthenticationFeature = (StringUtils.isNotBlank(username) && StringUtils.isNotBlank(password)) ? CustomAuthenticationFeature.create(username, password) : null;
        if (customAuthenticationFeature != null) {
            webTarget.register(customAuthenticationFeature);
            webTarget.property(ApacheClientProperties.REQUEST_CONFIG,
                    RequestConfig.custom().setCircularRedirectsAllowed(true).build());
        }
    }

    /**
     * 更新全局配置
     *
     * @param serverSettingsForm 全局配置
     * @throws Exception 异常
     */
    public void updateServerSettings(ServerSettingsForm serverSettingsForm) throws Exception {
        configurationManagementService.setBaseUrl(serverSettingsForm.getBaseUrl());
        configurationManagementService.setPort(serverSettingsForm.getPort());
        configurationManagementService.setKbps(serverSettingsForm.getKbps());
        configurationManagementService.setSliceMbSize(serverSettingsForm.getSliceMbSize());
        configurationManagementService.setInstanceName(serverSettingsForm.getInstanceName());
        if (serverSettingsForm.getCorsConfigurationForm() != null) {
            configurationManagementService.setCorsAllowedOrigins(
                    serverSettingsForm.getCorsConfigurationForm().getAllowedOrigins()
            );
        }
        if (serverSettingsForm.getSmtpConfigurationForm() != null) {
            // SMTP settings
            configurationManagementService.setSmtpSettings(
                    serverSettingsForm.getSmtpConfigurationForm().getMutableSmtpConfiguration()
            );
        }
        if (serverSettingsForm.getProxyConfigurationForm() != null) {
            // Global Proxy settings
            configurationManagementService.setProxyConfiguration(
                    null, null, serverSettingsForm.getProxyConfigurationForm().getMutableProxyConfiguration()
            );
        }
        if (serverSettingsForm.getAdvancedConfigurationForm() != null) {
            configurationManagementService.setAdvancedConfiguration(serverSettingsForm.getAdvancedConfigurationForm().getMutableProxyConfiguration());
            if (Boolean.FALSE.equals(serverSettingsForm.getAdvancedConfigurationForm().getAllowAnonymous())) {
                authorizationConfigService.clearPrivilegesAnonymous();
                updateAnonymous();
            } else if (Boolean.TRUE.equals(serverSettingsForm.getAdvancedConfigurationForm().getAllowAnonymous())) {
                authorizationConfigService.addPrivilegesToAnonymous(Lists.newArrayList(Privileges.ARTIFACTS_RESOLVE, Privileges.SEARCH_ARTIFACTS, Privileges.ARTIFACTS_VIEW, Privileges.CONFIGURATION_VIEW_METADATA_CONFIGURATION));
                updateAnonymous();
            }
        }
    }

    public void resolveS3Bucket() {
        AdvancedConfiguration advancedConfiguration = configurationManagementService.getConfiguration().getAdvancedConfiguration();
        if (Objects.isNull(advancedConfiguration)) {
            return;
        }
        String globalS3Bucket = advancedConfiguration.getGlobalS3Bucket();
        if (StringUtils.isBlank(globalS3Bucket)) {
            return;
        }
        globalS3Bucket = GlobalConstants.SEPARATOR + globalS3Bucket;
        for (Map.Entry<String, StorageDto> entry : configurationManagementService.getMutableConfigurationClone().getStorages().entrySet()) {
            try {
                StorageDto storage = entry.getValue();
                if (!StorageProviderEnum.S3.getType().equals(storage.getStorageProvider())) {
                    continue;
                }
                String storageId = storage.getId();
                String sourceStorageBasedir = storage.getBasedir();
                String storageBasedir = storage.getBasedir();
                if (StringUtils.isBlank(storageBasedir)) {
                    log.warn("Storage [{}] basedir is null", storageId);
                    continue;
                }
                if (storageBasedir.startsWith(globalS3Bucket)) {
                    storageBasedir = storageBasedir.replace(globalS3Bucket, "");
                    storage.setBasedir(storageBasedir);
                    //更新存储空间basedir
                    configurationManagementService.updateStorageBasedir(storage);
                    log.info("Storage [{}] basedir [{}] change to [{}]", storageId, sourceStorageBasedir, storageBasedir);
                }
                final Map<String, ? extends Repository> repositories = storage.getRepositories();
                for (Repository repository : repositories.values()) {
                    RepositoryDto repositoryDto = (RepositoryDto) repository;
                    String repositoryId = repositoryDto.getId();
                    String sourceRepositoryBasedir = repositoryDto.getBasedir();
                    String repositoryBasedir = repositoryDto.getBasedir();
                    if (repositoryBasedir.startsWith(globalS3Bucket)) {
                        repositoryBasedir = repositoryBasedir.replace(globalS3Bucket, "");
                        repositoryDto.setBasedir(repositoryBasedir);
                        //更新仓库basedir
                        configurationManagementService.setRepositoryBasedir(storage.getId(), repositoryDto);
                        log.info("Storage [{}] repository [{}] basedir [{}] change to [{}]", storageId, repositoryId, sourceRepositoryBasedir, repositoryBasedir);
                    }
                }
            } catch (Exception ex) {
                log.error("Storage [{}] resolveS3Bucket error [{}]", entry.getKey(), ExceptionUtils.getStackTrace(ex));
            }
        }
    }

    private void updateAnonymous() {
        List<GrantedAuthority> authorities = AuthorityUtils.createAuthorityList("ROLE_ANONYMOUS");
        Role role = authoritiesProvider.getRuntimeRole(SystemRole.ANONYMOUS.name());
        authorities.addAll(role.getAccessModel().getApiAuthorities());
        anonymousAuthenticationFilter.getAuthorities().clear();
        anonymousAuthenticationFilter.getAuthorities().addAll(authorities);
    }

    public void handleStorageProvider() throws IOException {
        for (Map.Entry<String, StorageDto> entry : configurationManagementService.getMutableConfigurationClone().getStorages().entrySet()) {
            StorageDto storage = entry.getValue();
            storageManagementService.handleStorageProvider(storage);
        }
    }

    public boolean isRepositoryResolvable(Repository repository) {
        final boolean isInService = repository.isInService();
        if (!isInService) {
            log.info("- Repository [{}] is not in service, skipping...",
                    repository.getStorageIdAndRepositoryId());
            return false;
        }
        return true;
    }

    public boolean hasAdmin() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return false;
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return false;
        }
        SpringSecurityUser userDetails = (SpringSecurityUser) o;
        if (CollectionUtils.isEmpty(userDetails.getRoles())) {
            return false;
        }
        return userDetails.getRoles().stream().anyMatch(item -> SystemRole.ADMIN.name().equals(item.getName()));
    }

    public SpringSecurityUser loginUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return null;
        }
        return (SpringSecurityUser) authentication.getPrincipal();
    }

    public String loginUsername() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.isNull(authentication)) {
            return "";
        }
        Object o = authentication.getPrincipal();
        if (!(o instanceof SpringSecurityUser)) {
            return "";
        }
        SpringSecurityUser springSecurityUser = (SpringSecurityUser) authentication.getPrincipal();
        if (Objects.isNull(springSecurityUser)) {
            return "";
        }
        return springSecurityUser.getUsername();
    }

    public void handlerRole(String roleInfo) {
        authorizationConfigService.handlerRole(roleInfo);
    }

    public ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(Integer corePoolSize, Integer maxPoolSize, Integer queueCapacity, Integer keepAliveSeconds, String threadNamePrefix, Integer awaitTerminationSeconds, RejectedExecutionHandler rejectedExecutionHandler) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        int availableCores = getAvailableCores();
        log.info("Current available cpu cores [{}]", availableCores);
        if (availableCores < 8) {
            availableCores = 8;
            log.info("Modify available cpu cores [{}]", availableCores);
        }
        if (corePoolSize > availableCores) {
            executor.setCorePoolSize(availableCores);
            executor.setMaxPoolSize(availableCores);
        } else {
            executor.setCorePoolSize(corePoolSize);
            executor.setMaxPoolSize(maxPoolSize);
        }
        Integer maxQueueCapacity = 100000000;
        if (queueCapacity > maxQueueCapacity) {
            queueCapacity = maxQueueCapacity;
        }
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(keepAliveSeconds);
        executor.setThreadNamePrefix(threadNamePrefix);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(awaitTerminationSeconds);
        if (Objects.isNull(rejectedExecutionHandler)) {
            rejectedExecutionHandler = new ThreadPoolExecutor.CallerRunsPolicy();
        }
        executor.setRejectedExecutionHandler(rejectedExecutionHandler);
        executor.initialize();
        log.info("Thread pool name [{}] core size [{}] max size [{}] queue capacity [{}]", executor.getThreadNamePrefix(), executor.getCorePoolSize(), executor.getMaxPoolSize(), queueCapacity);
        return executor;
    }

    public int getAvailableCores() {
        return Runtime.getRuntime().availableProcessors();
    }


    public Integer getConnectTimeout() {
        int connectTimeout = GlobalConstants.DEFAULT_CONTENT_TIME;
        String key = "REMOTE_REPOSITORY_CONNECT_TIMEOUT";
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(value)) {
            connectTimeout = Integer.parseInt(value);
        }
        return connectTimeout * 1000;
    }

    public Integer getReadTimeout() {
        int readTimeout = GlobalConstants.DEFAULT_READ_TIME;
        String key = "REMOTE_REPOSITORY_READ_TIMEOUT";
        String value = distributedCacheComponent.get(key);
        if (StringUtils.isNotBlank(value)) {
            readTimeout = Integer.parseInt(value);
        }
        return readTimeout * 1000;
    }

    /**
     * build ThreadPoolTaskExecutor
     *
     * @param threadNamePrefix threadNamePrefix
     * @param corePoolSize     corePoolSize
     * @param maxPoolSize      maxPoolSize
     * @return ThreadPoolTaskExecutor
     */
    public ThreadPoolTaskExecutor buildThreadPoolTaskExecutor(String threadNamePrefix, Integer corePoolSize, Integer maxPoolSize) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePoolSize);
        executor.setMaxPoolSize(maxPoolSize);
        executor.setQueueCapacity(10000);
        executor.setKeepAliveSeconds(120);
        executor.setThreadNamePrefix(threadNamePrefix + "_");
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(6);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        return executor;
    }
}
