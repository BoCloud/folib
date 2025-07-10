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

import com.folib.configuration.*;
import com.folib.storage.repository.*;
import com.google.common.collect.Sets;
import com.folib.client.MutableRemoteRepositoryRetryArtifactDownloadConfiguration;
import com.folib.constant.GlobalConstants;
import com.folib.event.repository.RepositoryEvent;
import com.folib.event.repository.RepositoryEventListenerRegistry;
import com.folib.event.repository.RepositoryEventTypeEnum;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.providers.layout.LayoutProvider;
import com.folib.providers.layout.LayoutProviderRegistry;
import com.folib.service.ProxyRepositoryConnectionPoolConfigurationService;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.RepositoryManagementService;
import com.folib.storage.Storage;
import com.folib.storage.StorageDto;
import com.folib.storage.routing.MutableRoutingRule;
import com.folib.storage.routing.MutableRoutingRules;
import com.folib.util.CacheUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.SerializationUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.convert.ConversionService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;
import java.io.IOException;
import java.lang.reflect.UndeclaredThrowableException;
import java.net.URI;
import java.nio.file.Files;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Slf4j
@Service
public class ConfigurationManagementServiceImpl
        implements ConfigurationManagementService {

    private final ReadWriteLock configurationLock = new ReentrantReadWriteLock();
    @Lazy
    @Inject
    private ConfigurationFileManager configurationFileManager;
    @Lazy
    @Inject
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;

    @Inject
    @Lazy
    private LayoutProviderRegistry layoutProviderRegistry;
    @Lazy
    @Inject
    private ProxyRepositoryConnectionPoolConfigurationService proxyRepositoryConnectionPoolConfigurationService;

    @Inject
    @Lazy
    private PlatformTransactionManager transactionManager;
    @Lazy
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Lazy
    @Inject
    private RepositoryManagementService repositoryManagementService;
    @Lazy
    @Inject
    private ConversionService conversionService;

    /**
     * Yes, this is a state object.
     * It is protected by the {@link #configurationLock} here
     * and should not be exposed to the world.
     *
     * @see #getConfiguration()
     */
    private MutableConfiguration configuration;

    @PostConstruct
    public void init() {
        new TransactionTemplate(transactionManager).execute((s) -> doInit());
    }

    private Object doInit() {
        MutableConfiguration configuration;
        try {
            configuration = configurationFileManager.read();
            setConfiguration(configuration);
            setRepositoryArtifactCoordinateValidators();
        } catch (IOException e) {
            throw new UndeclaredThrowableException(e);
        }

        return null;
    }

    @Override
    public MutableConfiguration getMutableConfigurationClone() {
        final Lock readLock = configurationLock.readLock();
        readLock.lock();

        try {
            return SerializationUtils.clone(configuration);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public Configuration getConfiguration() {
        final Lock readLock = configurationLock.readLock();
        readLock.lock();

        try {
            return new Configuration(configuration);
        } finally {
            readLock.unlock();
        }
    }

    @Override
    public void setConfiguration(MutableConfiguration newConf) throws IOException {
        Objects.requireNonNull(newConf, "Configuration cannot be null");

        modifyInLock(configuration -> {
            ConfigurationManagementServiceImpl.this.configuration = newConf;
            try {
                setProxyRepositoryConnectionPoolConfigurations();
                setRepositoryStorageRelationships();
                setAllows();
            } catch (IOException e) {
                throw new UndeclaredThrowableException(e);
            }
        });
    }

    @Override
    public void setInstanceName(String instanceName) throws IOException {
        modifyInLock(configuration -> configuration.setInstanceName(instanceName));
    }

    @Override
    public void setBaseUrl(String baseUrl) throws IOException {
        modifyInLock(configuration -> {
            String finalBaseUrl = baseUrl;
            if (!finalBaseUrl.endsWith(GlobalConstants.SEPARATOR)) {
                finalBaseUrl = finalBaseUrl + GlobalConstants.SEPARATOR;
            }
            configuration.setBaseUrl(finalBaseUrl);
            CacheUtil<String, URI> cacheUtil = CacheUtil.getInstance();
            cacheUtil.remove("uri");
        });
    }

    @Override
    public void setPort(int port) throws IOException {
        modifyInLock(configuration -> configuration.setPort(port));
    }

    @Override
    public void setKbps(Integer kbps) throws IOException {
        modifyInLock(configuration -> configuration.setKbps(kbps));
    }

    @Override
    public void setSliceMbSize(Long sliceMbSize) throws IOException {
        modifyInLock(configuration -> configuration.setSliceMbSize(sliceMbSize));
    }

    @Override
    public void setAdvancedConfiguration(MutableAdvancedConfiguration advancedConfiguration) throws IOException {
        modifyInLock(configuration ->
        {
            String globalS3Bucket = configuration.getAdvancedConfiguration().getGlobalS3Bucket();
            if (StringUtils.isNotBlank(globalS3Bucket)) {
                advancedConfiguration.setGlobalS3Bucket(globalS3Bucket);
            }
            configuration.setAdvancedConfiguration(advancedConfiguration);
        });
    }

    @Override
    public void setProxyConfiguration(String storageId,
                                      String repositoryId,
                                      MutableProxyConfiguration proxyConfiguration) throws IOException {
        modifyInLock(configuration ->
        {
            if (storageId != null && repositoryId != null) {
                configuration.getStorage(storageId)
                        .getRepository(repositoryId)
                        .setProxyConfiguration(proxyConfiguration);
            } else {
                configuration.setProxyConfiguration(proxyConfiguration);
            }
        });
    }

    @Override
    public void updateStorage(StorageDto storage) throws IOException {
        StorageDto storageDto = configuration.getStorage(storage.getId());
//        storageDto.setUsers(storage.getUsers());
        storageDto.setAdmin(storage.getAdmin());
        if (StringUtils.isBlank(storageDto.getStorageProvider()) && StringUtils.isNotBlank(storage.getStorageProvider())) {
            storageDto.setStorageProvider(storage.getStorageProvider());
        }
        if (!storage.getRepositories().isEmpty()) {
            storage.getRepositories().values().forEach(repository -> {
                RepositoryDto repositoryDto = conversionService.convert(repository, RepositoryDto.class);
                storageDto.addRepository(repositoryDto);
            });
        }
//        checkUsersContainsAdmin(storageDto);
        modifyInLock(configuration -> configuration.addStorage(storageDto));
    }

    @Override
    public void updateStorageBasedir(StorageDto storage) throws IOException {
        StorageDto storageDto = configuration.getStorage(storage.getId());
        if (StringUtils.isNotBlank(storage.getBasedir())) {
            storageDto.setBasedir(storage.getBasedir());
            modifyInLock(configuration -> configuration.addStorage(storageDto));
        }
    }

    @Override
    public void createStorage(StorageDto storage) throws IOException {
//        checkUsersContainsAdmin(storage);
        modifyInLock(configuration -> configuration.addStorage(storage));
    }

    @Override
    public void addStorageIfNotExists(StorageDto storage) throws IOException {
        modifyInLock(configuration -> configuration.addStorageIfNotExist(storage));
    }

    @Override
    public void removeStorage(String storageId) throws IOException {
        modifyInLock(configuration -> configuration.getStorages().remove(storageId));
    }


    @Override
    public void saveRepository(String storageId,
                               RepositoryDto repository) throws IOException {
        modifyInLock(configuration ->
        {
            final StorageDto storage = configuration.getStorage(storageId);
            Repository oldRepository = storage.getRepository(repository.getId());
            if (Objects.nonNull(oldRepository)) {
                repository.setBasedir(oldRepository.getBasedir());
                repository.setStorageProvider(oldRepository.getStorageProvider());
            }
            repository.setStorage(storage);
            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(
                    repository.getLayout());
            if (Objects.nonNull(layoutProvider) && CollectionUtils.isEmpty(repository.getArtifactCoordinateValidators())) {
                repository.setArtifactCoordinateValidators(layoutProvider.getDefaultArtifactCoordinateValidators());
            }
            RepositoryDto repositoryDto = storage.getRepository(repository.getId());
            if (Objects.nonNull(repositoryDto) && Objects.isNull(repository.getUnionRepositoryConfig())) {
                repository.setUnionRepositoryConfiguration(repositoryDto.getUnionRepositoryConfiguration());
            }
            storage.addRepository(repository);
            if (repository.isEligibleForCustomConnectionPool()) {
                proxyRepositoryConnectionPoolConfigurationService.setMaxPerRepository(
                        repository.getRemoteRepository().getUrl(),
                        repository.getHttpConnectionPool().getAllocatedConnections());
            }
            clearCacheRepository(storageId, repository.getId());
        });
    }

    @Override
    public void setRepositoryBasedir(String storageId, RepositoryDto repository) throws IOException {
        modifyInLock(configuration ->
        {
            final StorageDto storage = configuration.getStorage(storageId);
            RepositoryDto repositoryDto = storage.getRepository(repository.getId());
            if (StringUtils.isNotBlank(repository.getBasedir())) {
                repositoryDto.setBasedir(repository.getBasedir());
                storage.addRepository(repositoryDto);
                clearCacheRepository(storageId, repositoryDto.getId());
            }
        });
    }

    @Override
    public void removeRepositoryFromAssociatedGroups(String storageId,
                                                     String repositoryId) throws IOException {
        modifyInLock(configuration ->
        {
            List<Repository> includedInGroupRepositories = getConfiguration().getGroupRepositoriesContaining(
                    storageId, repositoryId);
            if (!includedInGroupRepositories.isEmpty()) {
                for (Repository repository : includedInGroupRepositories) {
                    configuration.getStorage(repository.getStorage().getId())
                            .getRepository(repository.getId())
                            .getGroupRepositories().remove(repositoryId);
                }
            }
        });
    }

    @Override
    public void addRepositoryVulnerabilityWhites(String storageId, String repositoryId, Set<String> whites) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            checkPlatformVulnerability(whites, repository);
            repository.addVulnerabilityWhites(whites);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void removeRepositoryVulnerabilityWhites(String storageId, String repositoryId, Set<String> whites) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            repository.removeVulnerabilityWhites(whites);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void addRepositoryVulnerabilityBlacks(String storageId, String repositoryId, Set<String> blacks) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            checkPlatformVulnerability(blacks, repository);
            repository.addVulnerabilityBlacks(blacks);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void removeRepositoryVulnerabilityBlacks(String storageId, String repositoryId, Set<String> blacks) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            repository.removeVulnerabilityBlacks(blacks);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void setRepositoryVulnerabilityWhites(String storageId, String repositoryId, Set<String> whites) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            checkPlatformVulnerability(whites, repository);
            repository.setVulnerabilityWhites(whites);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void setRepositoryVulnerabilityBlacks(String storageId, String repositoryId, Set<String> blacks) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            checkPlatformVulnerability(blacks, repository);
            repository.setVulnerabilityBlacks(blacks);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void removeRepository(String storageId,
                                 String repositoryId) throws IOException {
        modifyInLock(configuration -> {
            configuration.getStorage(storageId).removeRepository(repositoryId);
            try {
                removeRepositoryFromAssociatedGroups(storageId, repositoryId);
            } catch (IOException e) {
                throw new UndeclaredThrowableException(e);
            }
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void setProxyRepositoryMaxConnections(String storageId,
                                                 String repositoryId,
                                                 int numberOfConnections) throws IOException {
        modifyInLock(configuration ->
        {
            RepositoryDto repository = configuration.getStorage(storageId).getRepository(repositoryId);
            if (repository.getHttpConnectionPool() == null) {
                repository.setHttpConnectionPool(new MutableHttpConnectionPool());
            }

            repository.getHttpConnectionPool().setAllocatedConnections(numberOfConnections);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public MutableRoutingRules getRoutingRules() {
        return getMutableConfigurationClone().getRoutingRules();
    }

    @Override
    public MutableRoutingRule getRoutingRule(UUID uuid) {
        return getMutableConfigurationClone().getRoutingRules()
                .getRules()
                .stream()
                .filter(r -> r.getUuid().equals(uuid))
                .findFirst()
                .orElse(null);
    }

    @Override
    public boolean updateRoutingRule(UUID uuid,
                                     MutableRoutingRule routingRule) throws IOException {
        final MutableBoolean result = new MutableBoolean();
        modifyInLock(configuration -> configuration.getRoutingRules()
                .getRules()
                .stream()
                .filter(r -> r.getUuid().equals(uuid))
                .findFirst()
                .ifPresent(r -> result.setValue(r.updateBy(routingRule))));

        return result.isTrue();
    }

    @Override
    public boolean addRoutingRule(MutableRoutingRule routingRule) throws IOException {
        final MutableBoolean result = new MutableBoolean();
        modifyInLock(configuration ->
        {
            routingRule.setUuid(UUID.randomUUID());
            result.setValue(configuration.getRoutingRules()
                    .getRules()
                    .add(routingRule));
        });

        return result.isTrue();
    }

    @Override
    public boolean removeRoutingRule(UUID uuid) throws IOException {
        final MutableBoolean result = new MutableBoolean();
        modifyInLock(configuration ->
        {
            configuration.getRoutingRules()
                    .getRules()
                    .stream()
                    .filter(r -> r.getUuid().equals(uuid))
                    .findFirst()
                    .ifPresent(r -> result.setValue(configuration.getRoutingRules()
                            .getRules()
                            .remove(r)));

        });

        return result.isTrue();
    }

    @Override
    public void addRepositoryToGroup(String storageId,
                                     String repositoryId,
                                     String repositoryGroupMemberId) throws IOException {
        modifyInLock(configuration ->
        {
            final RepositoryDto repository = configuration.getStorage(storageId)
                    .getRepository(repositoryId);
            repository.addRepositoryToGroup(repositoryGroupMemberId);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    private void setAllows() throws IOException {
        modifyInLock(configuration ->
        {
            final Map<String, StorageDto> storages = configuration.getStorages();

            if (storages != null && !storages.isEmpty()) {
                for (StorageDto storage : storages.values()) {
                    if (storage.getRepositories() != null && !storage.getRepositories().isEmpty()) {
                        for (Repository repository : storage.getRepositories().values()) {
                            RepositoryDto mutableRepository = (RepositoryDto) repository;
                            if (repository.getType().equals(RepositoryTypeEnum.GROUP.getType())) {
                                mutableRepository.setAllowsDeletion(false);
                                mutableRepository.setAllowsDeployment(false);
                                mutableRepository.setAllowsRedeployment(false);
                            }
                            if (repository.getType().equals(RepositoryTypeEnum.PROXY.getType())) {
                                mutableRepository.setAllowsDeployment(false);
                                mutableRepository.setAllowsRedeployment(false);
                            }
                        }
                    }
                }
            }
        }, false);
    }

    /**
     * Sets the repository <--> storage relationships explicitly, as initially, when these are deserialized from the
     * XML, they have no such relationship.
     *
     * @throws IOException
     */
    private void setRepositoryStorageRelationships() throws IOException {
        modifyInLock(configuration ->
        {
            final Map<String, StorageDto> storages = configuration.getStorages();

            if (storages != null && !storages.isEmpty()) {
                for (StorageDto storage : storages.values()) {
                    if (storage.getRepositories() != null && !storage.getRepositories().isEmpty()) {
                        for (Repository repository : storage.getRepositories().values()) {
                            ((RepositoryDto) repository).setStorage(storage);
                        }
                    }
                }
            }
        }, false);
    }

    @Override
    public void setRepositoryArtifactCoordinateValidators() throws IOException {
        modifyInLock(configuration ->
        {
            final Map<String, StorageDto> storages = configuration.getStorages();

            if (storages != null && !storages.isEmpty()) {
                for (StorageDto storage : storages.values()) {
                    if (storage.getRepositories() != null && !storage.getRepositories().isEmpty()) {
                        for (Repository repository : storage.getRepositories().values()) {
                            LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(
                                    repository.getLayout());

                            // Generally, this should not happen. However, there are at least two cases where it may occur:
                            // 1) During testing -- various modules are not added as dependencies and a layout provider
                            //    is thus not registered.
                            // 2) Syntax error, or some other mistake leading to an incorrectly defined layout
                            //    for a repository.
                            if (layoutProvider != null) {
                                @SuppressWarnings("unchecked")
                                Set<String> defaultArtifactCoordinateValidators = layoutProvider.getDefaultArtifactCoordinateValidators();
                                if ((repository.getArtifactCoordinateValidators() == null ||
                                        (repository.getArtifactCoordinateValidators() != null &&
                                                repository.getArtifactCoordinateValidators().isEmpty())) &&
                                        defaultArtifactCoordinateValidators != null) {
                                    ((RepositoryDto) repository).setArtifactCoordinateValidators(defaultArtifactCoordinateValidators);
                                }
                            }
                        }
                    }
                }
            }
        });
    }

    @Override
    public void putInService(final String storageId,
                             final String repositoryId) throws IOException {
        modifyInLock(configuration ->
        {
            configuration.getStorage(storageId)
                    .getRepository(repositoryId)
                    .setStatus(RepositoryStatusEnum.IN_SERVICE.getStatus());

            RepositoryEvent event = new RepositoryEvent(storageId,
                    repositoryId,
                    RepositoryEventTypeEnum.EVENT_REPOSITORY_PUT_IN_SERVICE
                            .getType());

            repositoryEventListenerRegistry.dispatchEvent(event);
        });
    }

    @Override
    public void putOutOfService(final String storageId,
                                final String repositoryId) throws IOException {
        modifyInLock(configuration ->
        {
            configuration.getStorage(storageId)
                    .getRepository(repositoryId)
                    .setStatus(RepositoryStatusEnum.OUT_OF_SERVICE.getStatus());

            RepositoryEvent event = new RepositoryEvent(storageId,
                    repositoryId,
                    RepositoryEventTypeEnum.EVENT_REPOSITORY_PUT_OUT_OF_SERVICE
                            .getType());

            repositoryEventListenerRegistry.dispatchEvent(event);
        });
    }

    @Override
    public void setArtifactMaxSize(final String storageId,
                                   final String repositoryId,
                                   final long value) throws IOException {
        modifyInLock(configuration ->
        {
            configuration.getStorage(storageId)
                    .getRepository(repositoryId)
                    .setArtifactMaxSize(value);
            clearCacheRepository(storageId, repositoryId);
        });
    }

    @Override
    public void set(final MutableRemoteRepositoryRetryArtifactDownloadConfiguration remoteRepositoryRetryArtifactDownloadConfiguration) throws IOException {
        modifyInLock(configuration ->
        {
            configuration.getRemoteRepositoriesConfiguration()
                    .setRetryArtifactDownloadConfiguration(
                            remoteRepositoryRetryArtifactDownloadConfiguration);
        });
    }

    @Override
    public void addRepositoryArtifactCoordinateValidator(final String storageId,
                                                         final String repositoryId,
                                                         final String alias) throws IOException {
        modifyInLock(configuration ->
        {
            configuration.getStorage(storageId).getRepository(
                    repositoryId).getArtifactCoordinateValidators().add(alias);
        });
    }

    @Override
    public boolean removeRepositoryArtifactCoordinateValidator(final String storageId,
                                                               final String repositoryId,
                                                               final String alias) throws IOException {
        final MutableBoolean result = new MutableBoolean();
        modifyInLock(config ->
        {
            result.setValue(config.getStorage(storageId).getRepository(
                    repositoryId).getArtifactCoordinateValidators().remove(alias));
            clearCacheRepository(storageId, repositoryId);
        });

        return result.isTrue();
    }

    @Override
    public void setCorsAllowedOrigins(final List<String> allowedOrigins) throws IOException {
        modifyInLock(configuration ->
        {
            ArrayList origins;

            if (CollectionUtils.isEmpty(allowedOrigins)) {
                origins = new ArrayList<>();
            } else {
                origins = new ArrayList<>(allowedOrigins);
            }

            configuration.getCorsConfiguration()
                    .setAllowedOrigins(origins);
        });
    }

    @Override
    public List<Repository> getRepositoriesWithLayout(String storageId, String layout) {
        return getConfiguration().getRepositoriesWithLayout(storageId, layout);
    }

    @Override
    public List<Repository> getRepositoriesWithType(String storageId, String repositoryType) {
        return getConfiguration().getRepositoriesWithType(storageId, repositoryType);
    }

    @Override
    public void addOrUpdateMetadataConfiguration(MutableMetadataConfiguration mutableMetadataConfiguration) throws IOException {
        modifyInLock(configuration -> configuration.addOrUpdateMetadataConfiguration(mutableMetadataConfiguration));
    }

    @Override
    public void deleteMetadataConfig(String key) throws IOException {
        modifyInLock(configuration -> configuration.getMetadataConfiguration().remove(key));
    }

    @Override
    public void setWebhookConfiguration(Map<String, MutableWebhookConfiguration> webhookConfiguration) throws IOException {
        modifyInLock(configuration ->
        {
            configuration.setWebhookConfiguration(webhookConfiguration);
        });
    }

    @Override
    public void addWebhookConfiguration(MutableWebhookConfiguration mutableWebhookConfiguration) throws IOException {
        modifyInLock(configuration ->
        {
            MutableWebhookConfiguration sourceMutableWebhookConfiguration = configuration.getWebhookConfiguration().get(mutableWebhookConfiguration.getUuid());
            if (Objects.nonNull(sourceMutableWebhookConfiguration)) {
                throw new IllegalArgumentException(String.format("add webhook configuration %s existent", mutableWebhookConfiguration.getUuid()));
            }
            boolean flag = CollectionUtils.isNotEmpty(configuration.getWebhookConfiguration().values()) && configuration.getWebhookConfiguration().values().stream().anyMatch(item -> item.getUrl().equals(mutableWebhookConfiguration.getUrl()));
            if (flag) {
                throw new IllegalArgumentException(String.format("url %s existent", mutableWebhookConfiguration.getUrl()));
            }
            configuration.addOrUpdateWebhookConfiguration(mutableWebhookConfiguration);
        });
    }

    @Override
    public void updateWebhookConfiguration(MutableWebhookConfiguration mutableWebhookConfiguration) throws IOException {
        modifyInLock(configuration ->
        {
            MutableWebhookConfiguration sourceMutableWebhookConfiguration = configuration.getWebhookConfiguration().get(mutableWebhookConfiguration.getUuid());
            if (Objects.isNull(sourceMutableWebhookConfiguration)) {
                throw new IllegalArgumentException(String.format("update webhook configuration %s non existent", mutableWebhookConfiguration.getUuid()));
            }
            configuration.addOrUpdateWebhookConfiguration(mutableWebhookConfiguration);
        });
    }

    @Override
    public void deleteWebhookConfiguration(String uuid) throws IOException {
        MutableWebhookConfiguration sourceMutableWebhookConfiguration = configuration.getWebhookConfiguration().get(uuid);
        if (Objects.isNull(sourceMutableWebhookConfiguration)) {
            throw new IllegalArgumentException(String.format("delete webhook configuration %s non existent", uuid));
        }
        modifyInLock(configuration -> configuration.getWebhookConfiguration().remove(uuid));
    }

    @Override
    public void setUnionRepositoryConfiguration(String storageId, String repositoryId, MutableUnionRepositoryConfiguration mutableUnionRepositoryConfiguration) throws IOException {
        modifyInLock(configuration -> {
            if (storageId != null && repositoryId != null) {
                configuration.getStorage(storageId)
                        .getRepository(repositoryId)
                        .setUnionRepositoryConfiguration(mutableUnionRepositoryConfiguration);
            }
        });
    }

    @Override
    public void addOrUpdateRepository(String storageId, RepositoryDto repository) {
        Storage storage = getConfiguration().getStorage(storageId);
        if (storage != null) {
            if (repository.getArtifactMaxSize() == 0) {
                repository.setArtifactMaxSize(107374182400L);
            }
            String repositoryId = repository.getId();
            Repository existRepository = storage.getRepository(repositoryId);
            boolean result = Objects.nonNull(existRepository) && (!repository.getLayout().equals(existRepository.getLayout()) || (Objects.nonNull(existRepository.getSubLayout()) && !existRepository.getSubLayout().equals(repository.getSubLayout())));
            if (result) {
                //判断重复
                throw new RuntimeException("The repository id already exists");
            }
            try {
                log.info("Creating repository {}:{}...", storageId, repositoryId);
                saveRepository(storageId, repository);
                RepositoryDto repositoryDto = getMutableConfigurationClone().getStorage(storageId)
                        .getRepository(repositoryId);
                final RepositoryPath repositoryPath = repositoryPathResolver.resolve(new RepositoryData(repository));
                try {
                    if (!Files.exists(repositoryPath)) {
                        repositoryManagementService.createRepository(storageId, repositoryId);
                    }
                } catch (Exception ex) {
                    log.error("Failed to create the repository path {}!", repositoryId, ex);
                    try {
                        removeRepository(storageId, repositoryId);
                    } catch (Exception e) {
                        log.error("Failed to remove the repository {}!", repositoryId, e);
                    }
                    throw new RuntimeException(ex.getMessage());
                }
                if (Objects.isNull(existRepository) && !RepositoryTypeEnum.GROUP.getType().equals(repository.getType())) {
                    //初始化仓库数据
                    LayoutProvider layoutProvider = layoutProviderRegistry.getProvider(repositoryDto.getLayout());
                    layoutProvider.initData(storageId, repositoryId);
                }
            } catch (Exception e) {
                log.error("Failed to save the repository {}!", repositoryId, e);
            }
        } else {
            throw new IllegalArgumentException(String.format("storage %s not exist", storageId));
        }
    }

    private void setProxyRepositoryConnectionPoolConfigurations() throws IOException {
        modifyInLock(configuration ->
        {
            configuration.getStorages().values().stream()
                    .filter(storage -> MapUtils.isNotEmpty(storage.getRepositories()))
                    .flatMap(storage -> storage.getRepositories().values().stream())
                    .map(r -> (RepositoryDto) r)
                    .filter(RepositoryDto::isEligibleForCustomConnectionPool)
                    .forEach(repository -> proxyRepositoryConnectionPoolConfigurationService.setMaxPerRepository(repository.getRemoteRepository()
                                    .getUrl(),
                            repository.getHttpConnectionPool()
                                    .getAllocatedConnections()));
        }, false);
    }

    @Override
    public void setSmtpSettings(MutableSmtpConfiguration smtpConfiguration) throws IOException {
        modifyInLock(configuration -> {
            configuration.getSmtpConfiguration().setHost(smtpConfiguration.getHost());
            configuration.getSmtpConfiguration().setPort(smtpConfiguration.getPort());
            configuration.getSmtpConfiguration().setConnection(smtpConfiguration.getConnection());
            configuration.getSmtpConfiguration().setUsername(smtpConfiguration.getUsername());
            configuration.getSmtpConfiguration().setPassword(smtpConfiguration.getPassword());
        });
    }

    private void modifyInLock(final Consumer<MutableConfiguration> operation) throws IOException {
        modifyInLock(operation, true);
    }

    private void modifyInLock(final Consumer<MutableConfiguration> operation,
                              final boolean storeInFile) throws IOException {
        final Lock writeLock = configurationLock.writeLock();
        writeLock.lock();

        try {
            operation.accept(configuration);

            if (storeInFile) {
                configurationFileManager.store(configuration);
            }
        } finally {
            writeLock.unlock();
        }
    }

    private void checkUsersContainsAdmin(StorageDto storage) {
        if (StringUtils.isNotBlank(storage.getAdmin())) {
            if (Objects.isNull(storage.getUsers())) {
                storage.setUsers(Sets.newHashSet());
            }
            if (!storage.getUsers().contains(storage.getAdmin())) {
                storage.getUsers().add(storage.getAdmin());
            }
        }
    }

    private void checkPlatformVulnerability(Set<String> sets, RepositoryDto repositoryDto) {
        if (CollectionUtils.isEmpty(sets)) {
            return;
        }
//        Set<String> platformWhites = getVulnerabilityWhites();
//        List<String> platformWhitesContains = platformWhites.stream().filter(sets::contains).distinct().collect(Collectors.toList());
//        if (CollectionUtils.isNotEmpty(platformWhitesContains)) {
//            throw new RuntimeException(platformWhitesContains + "已在平台级别白名单中");
//        }
//        Set<String> platformBlacks = getVulnerabilityBlacks();
//        List<String> platformBlacksContains = platformBlacks.stream().filter(sets::contains).distinct().collect(Collectors.toList());
//        if (CollectionUtils.isNotEmpty(platformBlacksContains)) {
//            throw new RuntimeException(platformBlacksContains + "已在平台级别黑白名单中");
//        }
        //白名单
        List<String> whitesContains = repositoryDto.getVulnerabilityWhites().stream().filter(sets::contains).distinct().collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(whitesContains)) {
            throw new RuntimeException(whitesContains + "已在白名单中");
        }
        //黑名单
        List<String> blacksContains = repositoryDto.getVulnerabilityBlacks().stream().filter(sets::contains).distinct().collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(blacksContains)) {
            throw new RuntimeException(blacksContains + "已在黑白名单中");
        }
    }

    private void clearCacheRepository(String storageId, String repositoryId) {
        try {
            String key = String.format("%s:%s", storageId, repositoryId);
            CacheUtil<String, Repository> cacheUtil = CacheUtil.getInstance();
            cacheUtil.remove(key);
        } catch (Exception ex) {
            log.error("移除仓库缓存错误 storageId [{}] repositoryId [{}] error [{}]", storageId, repositoryId, ExceptionUtils.getStackTrace(ex));
        }
    }
}
