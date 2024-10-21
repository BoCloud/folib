package com.veadan.folib.storage.repository;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.veadan.folib.configuration.*;
import com.veadan.folib.json.MapValuesJsonSerializer;
import com.veadan.folib.json.StringArrayToMapJsonDeserializer;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageData;
import com.veadan.folib.storage.repository.remote.RemoteRepositoryData;
import com.veadan.folib.storage.repository.remote.RemoteRepositoryDto;
import com.veadan.folib.util.CustomStreamCollectors;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
import com.veadan.folib.yaml.repository.RepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.apache.commons.lang3.StringUtils;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonIgnoreProperties(value = {"storageId"}, allowGetters = true)
public class RepositoryData
        implements Repository {

    private String id;

    private String basedir;

    private String policy;

    private String storageProvider;

    private String layout;

    private String subLayout;

    private String type;

    private boolean secured;

    private String status;

    private long artifactMaxSize;

    private boolean trashEnabled;

    private boolean allowsForceDeletion;

    private boolean allowsDeployment;

    private boolean allowsRedeployment;

    private boolean allowsDeletion;

    private boolean allowsDirectoryBrowsing;

    private boolean checksumHeadersEnabled;

    private ProxyConfiguration proxyConfiguration;

    private RemoteRepositoryData remoteRepository;

    private HttpConnectionPool httpConnectionPool;

    private List<CustomConfiguration> customConfigurations;

    private CustomRepositoryConfiguration repositoryConfiguration;

    @JsonSerialize(using = MapValuesJsonSerializer.class)
    @JsonDeserialize(using = StringArrayToMapJsonDeserializer.class)
    private Map<String, String> groupRepositories;

    @JsonSerialize(using = MapValuesJsonSerializer.class)
    @JsonDeserialize(using = StringArrayToMapJsonDeserializer.class)
    private Map<String, String> artifactCoordinateValidators;

    /**
     * 白名单列表
     */
    private Set<String> vulnerabilityWhites;
    /**
     * 黑名单列表
     */
    private Set<String> vulnerabilityBlacks;

    /**
     * 仓库可见范围 1 存储空间内 2 公开
     */
    private Integer scope;

    /**
     * 是否允许匿名访问
     */
    private boolean allowAnonymous;

    /**
     * 联邦仓库配置
     */
    private UnionRepositoryConfiguration unionRepositoryConfiguration;

    /**
     * proxy 健康状态
     */
    private boolean healthStatus;

    @JsonIgnore
    private Storage storage;

    /**是否同步存储空间到其他节点*/
    private boolean syncEnabled;

    RepositoryData() {

    }

    public RepositoryData(final Repository delegate) {
        this(delegate, null);
    }

    public RepositoryData(final Repository delegate,
                          final Storage storage) {
        this.id = delegate.getId();
        this.policy = delegate.getPolicy();
        this.storageProvider = delegate.getStorageProvider();
        this.layout = delegate.getLayout();
        this.type = delegate.getType();
        this.secured = delegate.isSecured();
        this.status = delegate.getStatus();
        this.artifactMaxSize = delegate.getArtifactMaxSize();
        this.trashEnabled = delegate.isTrashEnabled();
        this.allowsForceDeletion = delegate.isAllowsForceDeletion();
        this.allowsDeployment = delegate.isAllowsDeployment();
        this.allowsRedeployment = delegate.isAllowsRedeployment();
        this.allowsDeletion = delegate.isAllowsDeletion();
        this.allowsDirectoryBrowsing = delegate.isAllowsDirectoryBrowsing();
        this.checksumHeadersEnabled = delegate.isChecksumHeadersEnabled();

        RepositoryDto mutableRepository = (RepositoryDto) delegate;
        this.proxyConfiguration = immuteProxyConfiguration(mutableRepository.getProxyConfiguration());
        this.remoteRepository = immuteRemoteRepository(mutableRepository.getRemoteRepository());
        this.httpConnectionPool = immuteHttpConnectionPool(mutableRepository.getHttpConnectionPool());
        this.customConfigurations = immuteCustomConfigurations(mutableRepository.getCustomConfigurations());
        this.repositoryConfiguration = immuteCustomRepositoryConfiguration(mutableRepository.getRepositoryConfiguration());

        this.groupRepositories = immuteGroupRepositories(delegate.getGroupRepositories());
        this.artifactCoordinateValidators = immuteArtifactCoordinateValidators(
                delegate.getArtifactCoordinateValidators());
        this.vulnerabilityWhites = immuteVulnerabilityWhites(
                delegate.getVulnerabilityWhites());
        this.vulnerabilityBlacks = immuteVulnerabilityBlacks(
                delegate.getVulnerabilityBlacks());
        this.storage = storage != null ? storage : immuteStorage(delegate.getStorage());
        this.basedir = delegate.getBasedir();
        this.subLayout = delegate.getSubLayout();
        this.scope = delegate.getScope();
        this.allowAnonymous = delegate.isAllowAnonymous();
        this.syncEnabled = delegate.isSyncEnabled();
        this.unionRepositoryConfiguration = immuteUnionRepositoryConfiguration(mutableRepository.getUnionRepositoryConfiguration());
        if (delegate.getHealthStatus() != null) {
            this.healthStatus = delegate.getHealthStatus();
        }
    }

    private ProxyConfiguration immuteProxyConfiguration(final MutableProxyConfiguration source) {
        return source != null ? new ProxyConfiguration(source) : null;
    }

    private RemoteRepositoryData immuteRemoteRepository(final RemoteRepositoryDto source) {
        return source != null ? new RemoteRepositoryData(source) : null;
    }

    private Map<String, String> immuteGroupRepositories(final Set<String> source) {
        return source != null ? ImmutableMap.copyOf(source.stream().collect(CustomStreamCollectors.toLinkedHashMap(e -> e, e -> e))) :
                Collections.emptyMap();
    }

    private Storage immuteStorage(final Storage source) {
        return source != null ? new StorageData(source) : null;
    }

    private HttpConnectionPool immuteHttpConnectionPool(final MutableHttpConnectionPool source) {
        return source != null ? new HttpConnectionPool(source) : null;
    }

    private List<CustomConfiguration> immuteCustomConfigurations(final List<MutableCustomConfiguration> source) {
        return source != null ?
                ImmutableList.copyOf(source.stream().map(MutableCustomConfiguration::getImmutable).collect(
                        Collectors.toList())) : Collections.emptyList();
    }

    private CustomRepositoryConfiguration immuteCustomRepositoryConfiguration(final CustomRepositoryConfigurationDto source) {
        return source != null ? source.getImmutable() : null;
    }

    private Map<String, String> immuteArtifactCoordinateValidators(final Set<String> source)
    {
        return source != null ? ImmutableMap.copyOf(source.stream().collect(CustomStreamCollectors.toLinkedHashMap(e -> e, e -> e))) :
                Collections.emptyMap();
    }

    private Set<String> immuteVulnerabilityWhites(final Set<String> source) {
        return source != null ? source: Collections.emptySet();
    }

    private Set<String> immuteVulnerabilityBlacks(final Set<String> source) {
        return source != null ? source: Collections.emptySet();
    }

    private UnionRepositoryConfiguration immuteUnionRepositoryConfiguration(final MutableUnionRepositoryConfiguration source) {
        return source != null ? new UnionRepositoryConfiguration(source) : null;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getBasedir() {
        return basedir;
    }

    @Override
    public String getPolicy() {
        return policy;
    }

    @Override
    public String getStorageProvider() {
        return storageProvider;
    }

    @Override
    public String getLayout() {
        return layout;
    }

    @Override
    public String getType() {
        return type;
    }

    @Override
    public boolean isSecured() {
        return secured;
    }

    @Override
    public String getStatus() {
        return status;
    }

    @Override
    public long getArtifactMaxSize() {
        return artifactMaxSize;
    }

    @Override
    public boolean isTrashEnabled() {
        return trashEnabled;
    }

    @Override
    public boolean isAllowsForceDeletion() {
        return allowsForceDeletion;
    }

    @Override
    public boolean isAllowsDeployment() {
        return allowsDeployment;
    }

    @Override
    public boolean isAllowsRedeployment() {
        return allowsRedeployment;
    }

    @Override
    public boolean isAllowsDeletion() {
        return allowsDeletion;
    }

    @Override
    public boolean isAllowsDirectoryBrowsing() {
        return allowsDirectoryBrowsing;
    }

    @Override
    public boolean isChecksumHeadersEnabled() {
        return checksumHeadersEnabled;
    }

    public ProxyConfiguration getProxyConfiguration() {
        return proxyConfiguration;
    }

    @Override
    public RemoteRepositoryData getRemoteRepository() {
        return remoteRepository;
    }

    @Override
    public ProxyConfiguration getProxyConfig() {
        return this.proxyConfiguration;
    }

    public HttpConnectionPool getHttpConnectionPool() {
        return httpConnectionPool;
    }

    public List<CustomConfiguration> getCustomConfigurations() {
        return customConfigurations;
    }

    @Override
    public RepositoryConfiguration getRepositoryConfiguration() {
        return repositoryConfiguration;
    }

    @Override
    @JsonIgnore
    public String getStorageIdAndRepositoryId() {
        StringJoiner storageAndRepositoryId = new StringJoiner(":");

        if (StringUtils.isNotBlank(getStorage().getId())) {
            storageAndRepositoryId.add(getStorage().getId());
        }

        if (StringUtils.isNotBlank(getId())) {
            storageAndRepositoryId.add(getId());
        }

        return storageAndRepositoryId.toString();
    }

    @Override
    public boolean isType(String compareType) {
        return type.equalsIgnoreCase(compareType);
    }

    @Override
    public Set<String> getGroupRepositories() {
        return groupRepositories.keySet();
    }

    @Override
    public Set<String> getArtifactCoordinateValidators() {
        return artifactCoordinateValidators.keySet();
    }

    @Override
    public Storage getStorage() {
        return storage;
    }

    /**
     * This field is mainly used in the UI so don't remove it!
     */
    @JsonGetter("storageId")
    public String getStorageId() {
        return this.storage != null ? this.storage.getId() : null;
    }

    @Override
    public boolean isHostedRepository() {
        return RepositoryTypeEnum.HOSTED.getType().equals(type);
    }

    @Override
    public boolean isProxyRepository() {
        return RepositoryTypeEnum.PROXY.getType().equals(type);
    }

    @Override
    public boolean isGroupRepository() {
        return RepositoryTypeEnum.GROUP.getType().equals(type);
    }

    @Override
    public boolean isInService() {
        return RepositoryStatusEnum.IN_SERVICE.getStatus().equalsIgnoreCase(getStatus());
    }

    @Override
    public boolean isAcceptsSnapshots() {
        return RepositoryPolicyEnum.ofPolicy(getPolicy()).acceptsSnapshots();
    }

    @Override
    public boolean isAcceptsReleases() {
        return RepositoryPolicyEnum.ofPolicy(getPolicy()).acceptsReleases();
    }

    @Override
    public Set<String> getVulnerabilityWhites() {
        return vulnerabilityWhites;
    }

    @Override
    public Set<String> getVulnerabilityBlacks() {
        return vulnerabilityBlacks;
    }

    @Override
    public String getSubLayout() {
        return subLayout;
    }

    @Override
    public Integer getScope() {
        return this.scope;
    }

    @Override
    public boolean isAllowAnonymous() {
        return allowAnonymous;
    }

    @Override
    public UnionRepositoryConfiguration getUnionRepositoryConfig() {
        return unionRepositoryConfiguration;
    }

    @Override
    public boolean isSyncEnabled() {
        return syncEnabled;
    }

    public void setSyncEnabled(boolean syncEnabled) {
        this.syncEnabled = syncEnabled;
    }

    @Override
    public Boolean getHealthStatus() {
        return healthStatus;
    }
}
