package com.veadan.folib.configuration;

import com.google.common.collect.ImmutableSortedMap;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageData;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.HttpConnectionPool;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.storage.routing.MutableRoutingRules;
import com.veadan.folib.storage.routing.RoutingRules;

import javax.annotation.concurrent.Immutable;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toMap;

/**
 * @author veadan
 */
@Immutable
public class Configuration {

    private final String id;

    private final String instanceName;

    private final String version;

    private final String revision;

    private final String baseUrl;

    private final int port;

    /**
     * 全局节点传输速率（KB/s）
     */
    private Integer kbps;

    /**
     * 节点传输切片大小（MB）,缺省值50MB
     */
    private Long sliceMbSize;

    private final ProxyConfiguration proxyConfiguration;

    private final SessionConfiguration sessionConfiguration;

    private final RemoteRepositoriesConfiguration remoteRepositoriesConfiguration;

    private final Map<String, Storage> storages;

    private final RoutingRules routingRules;

    private final CorsConfiguration corsConfiguration;

    private final SmtpConfiguration smtpConfiguration;

    /**
     * 元数据配置
     */
    private final Map<String, MetadataConfiguration> metadataConfiguration;

    /**
     * webhook配置
     */
    private final Map<String, WebhookConfiguration> webhookConfiguration;

    /**
     * 高级配置
     */
    private final AdvancedConfiguration advancedConfiguration;

    /**
     * 构造函数
     *
     * @param delegate 配置代理
     */

    public Configuration(final MutableConfiguration delegate) {

        id = delegate.getId();
        instanceName = delegate.getInstanceName();
        version = delegate.getVersion();
        revision = delegate.getRevision();
        baseUrl = delegate.getBaseUrl();
        port = delegate.getPort();
        kbps = delegate.getKbps();
        sliceMbSize = delegate.getSliceMbSize();
        proxyConfiguration = immuteProxyConfiguration(delegate.getProxyConfiguration());
        sessionConfiguration = immuteSessionConfiguration(delegate.getSessionConfiguration());
        remoteRepositoriesConfiguration = immuteRemoteRepositoriesConfiguration(
                delegate.getRemoteRepositoriesConfiguration());
        storages = immuteStorages(delegate.getStorages());
        routingRules = immuteRoutingRules(delegate.getRoutingRules());
        corsConfiguration = immuteCorsConfiguration(delegate.getCorsConfiguration());
        smtpConfiguration = immuteSmtpConfiguration(delegate.getSmtpConfiguration());
        metadataConfiguration = immuteMetadataConfiguration(delegate.getMetadataConfiguration());
        webhookConfiguration = immuteWebhookConfiguration(delegate.getWebhookConfiguration());
        advancedConfiguration = immuteAdvancedConfiguration(delegate.getAdvancedConfiguration());
    }

    private ProxyConfiguration immuteProxyConfiguration(final MutableProxyConfiguration source) {
        return source != null ? new ProxyConfiguration(source) : null;
    }

    private SessionConfiguration immuteSessionConfiguration(final MutableSessionConfiguration source) {
        return source != null ? new SessionConfiguration(source) : null;
    }

    private Map<String, Storage> immuteStorages(final Map<String, StorageDto> source) {
        return source != null ? ImmutableSortedMap.copyOf(source.entrySet().stream().collect(
                toMap(Map.Entry::getKey, e -> new StorageData(e.getValue())))) : Collections.emptyMap();
    }

    private RemoteRepositoriesConfiguration immuteRemoteRepositoriesConfiguration(final MutableRemoteRepositoriesConfiguration source) {
        return source != null ? new RemoteRepositoriesConfiguration(source) : null;
    }

    private RoutingRules immuteRoutingRules(final MutableRoutingRules source) {
        return source != null ? new RoutingRules(source) : null;
    }

    private CorsConfiguration immuteCorsConfiguration(final MutableCorsConfiguration source) {
        return source != null ? new CorsConfiguration(source) : null;
    }

    private SmtpConfiguration immuteSmtpConfiguration(final MutableSmtpConfiguration source) {
        return source != null ? new SmtpConfiguration(source) : null;
    }

    private Map<String, WebhookConfiguration> immuteWebhookConfiguration(final Map<String, MutableWebhookConfiguration> source) {
        return source != null ? ImmutableSortedMap.copyOf(source.entrySet().stream().collect(
                toMap(Map.Entry::getKey, e -> new WebhookConfiguration(e.getValue())))) : Collections.emptyMap();
    }

    private Map<String, MetadataConfiguration> immuteMetadataConfiguration(final Map<String, MutableMetadataConfiguration> source) {
        return source != null ? ImmutableSortedMap.copyOf(source.entrySet().stream().collect(
                toMap(Map.Entry::getKey, e -> new MetadataConfiguration(e.getValue())))) : Collections.emptyMap();
    }

    private AdvancedConfiguration immuteAdvancedConfiguration(final MutableAdvancedConfiguration source) {
        return source != null ? new AdvancedConfiguration(source) : null;
    }

    public String getId() {
        return id;
    }

    public String getInstanceName() {
        return instanceName;
    }

    public String getVersion() {
        return version;
    }

    public String getRevision() {
        return revision;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    public int getPort() {
        return port;
    }

    public Integer getKbps() {
        return kbps;
    }

    public Long getSliceMbSize() {
        return sliceMbSize;
    }

    public ProxyConfiguration getProxyConfiguration() {
        return proxyConfiguration;
    }

    public SessionConfiguration getSessionConfiguration() {
        return sessionConfiguration;
    }

    public RemoteRepositoriesConfiguration getRemoteRepositoriesConfiguration() {
        return remoteRepositoriesConfiguration;
    }

    public Map<String, Storage> getStorages() {
        return storages;
    }

    public Storage getStorage(final String storageId) {
        return storages.get(storageId);
    }

    public RoutingRules getRoutingRules() {
        return routingRules;
    }

    public List<Repository> getRepositoriesWithLayout(String storageId,
                                                      String layout) {
        Stream<? extends Repository> repositories;
        if (storageId != null) {
            Storage storage = getStorage(storageId);
            if (storage != null) {
                repositories = storage.getRepositories().values().stream();
            } else {
                return Collections.emptyList();
            }
        } else {
            repositories = getStorages().values().stream().flatMap(
                    storage -> storage.getRepositories().values().stream());
        }

        return repositories.filter(repository -> repository.getLayout().equals(layout))
                .collect(Collectors.toList());
    }

    public List<Repository> getRepositoriesWithType(String storageId,
                                                    String repositoryType) {
        Stream<? extends Repository> repositories;
        if (storageId != null) {
            Storage storage = getStorage(storageId);
            if (storage != null) {
                repositories = storage.getRepositories().values().stream();
            } else {
                return Collections.emptyList();
            }
        } else {
            repositories = getStorages().values().stream().flatMap(
                    storage -> storage.getRepositories().values().stream());
        }
        if (!"all".equals(repositoryType)) {
            return repositories.filter(repository -> repository.getType().equals(repositoryType))
                    .collect(Collectors.toList());
        }
        return repositories.collect(Collectors.toList());
    }

    public List<Repository> getRepositories() {
        List<Repository> repositories = new ArrayList<>();

        for (Storage storage : getStorages().values()) {
            repositories.addAll(storage.getRepositories().values());
        }

        return repositories;
    }

    public List<Repository> getGroupRepositories() {
        List<Repository> groupRepositories = new ArrayList<>();

        for (Storage storage : getStorages().values()) {
            groupRepositories.addAll(storage.getRepositories()
                    .values()
                    .stream()
                    .filter(repository -> repository.getType()
                            .equals(RepositoryTypeEnum.GROUP.getType()))
                    .collect(Collectors.toList()));
        }

        return groupRepositories;
    }

    public Repository getRepository(String storageId,
                                    String repositoryId) {
        return getStorage(storageId).getRepository(repositoryId);
    }

    public List<Repository> getGroupRepositoriesContaining(String storageId,
                                                           String repositoryId) {
        String storageAndRepositoryId = storageId + ":" + repositoryId;
        List<Repository> groupRepositories = getGroupRepositories();
        for (Iterator<Repository> it = groupRepositories.iterator(); it.hasNext(); ) {
            Repository repository = it.next();
            Optional<String> exists = repository.getGroupRepositories()
                    .stream()
                    .filter(groupName -> (groupName.equals(storageAndRepositoryId) ||
                            (repository.getStorage().getId().equals(storageId) && groupName.equals(repositoryId))))
                    .findFirst();
            if (!exists.isPresent()) {
                it.remove();
            }
        }
        return groupRepositories;
    }

    public HttpConnectionPool getHttpConnectionPoolConfiguration(String storageId,
                                                                 String repositoryId) {
        Repository repository = getStorage(storageId).getRepository(repositoryId);
        return ((RepositoryData) repository).getHttpConnectionPool();
    }

    public CorsConfiguration getCorsConfiguration() {
        return corsConfiguration;
    }

    public SmtpConfiguration getSmtpConfiguration() {
        return smtpConfiguration;
    }

    public Map<String, MetadataConfiguration> getMetadataConfiguration() {
        return metadataConfiguration;
    }

    public Map<String, WebhookConfiguration> getWebhookConfiguration() {
        return webhookConfiguration;
    }

    public AdvancedConfiguration getAdvancedConfiguration() {
        return advancedConfiguration;
    }
}
