package com.veadan.folib.storage.repository;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.veadan.folib.configuration.*;
import com.veadan.folib.providers.storage.FileSystemStorageProvider;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.remote.RemoteRepositoryDto;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.*;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RepositoryDto
        implements Serializable, Repository {

    private String id;

    private String basedir;

    private String policy = RepositoryPolicyEnum.MIXED.getPolicy();

    private String storageProvider = FileSystemStorageProvider.ALIAS;

    private String layout;

    private String subLayout;

    /**
     * 是否启用自定义布局
     */
    private boolean enableCustomLayout = false;

    /**
     * 自定义布局
     */
    private String customLayout;

    private String type = RepositoryTypeEnum.HOSTED.getType();

    private boolean secured;

    private String status = RepositoryStatusEnum.IN_SERVICE.getStatus();

    private long artifactMaxSize;

    private boolean trashEnabled;

    private boolean allowsForceDeletion;

    private boolean allowsDeployment = true;

    private boolean allowsRedeployment = true;

    private boolean allowsDeletion = true;

    private boolean allowsDirectoryBrowsing = true;

    private boolean checksumHeadersEnabled;

    private long storageMaxSize;

    private double storageThreshold;

    /**
     * The per-repository proxy settings that override the overall global proxy settings.
     */
    private MutableProxyConfiguration proxyConfiguration;

    private RemoteRepositoryDto remoteRepository;

    private MutableHttpConnectionPool httpConnectionPool;

    private List<MutableCustomConfiguration> customConfigurations = new ArrayList<>();

    private CustomRepositoryConfigurationDto repositoryConfiguration;

    @JsonDeserialize(as = LinkedHashSet.class)
    private Set<String> groupRepositories = new LinkedHashSet<>();

    private String groupDefaultRepository;

    public String getGroupDefaultRepository() {
        return groupDefaultRepository;
    }

    public void setGroupDefaultRepository(String groupDefaultRepository) {
        this.groupDefaultRepository = groupDefaultRepository;
    }

    private Set<String> artifactCoordinateValidators = new LinkedHashSet<>();

    /**
     * 白名单列表
     */
    private Set<String> vulnerabilityWhites = new LinkedHashSet<>();
    /**
     * 黑名单列表
     */
    private Set<String> vulnerabilityBlacks = new LinkedHashSet<>();

    /**
     * 仓库可见范围 1 存储空间内 2 公开
     */
    private Integer scope = 1;

    /**
     * 是否允许匿名访问
     */
    private boolean allowAnonymous = true;

    /**
     * 联邦仓库配置
     */
    private MutableUnionRepositoryConfiguration unionRepositoryConfiguration;

    /**
     * proxy 健康状态
     */
    @Getter
    private Boolean healthStatus;


    @JsonIgnore
    private StorageDto storage;

    /**是否同步存储空间到其他节点*/
    private boolean syncEnabled;

    private boolean syncJfrog;

    private String migrateId;

    private int syncStatus;

    private int totalArtifact;

    public boolean isSyncJfrog() {
        return syncJfrog;
    }

    public int getTotalArtifact() {
        return totalArtifact;
    }

    public void setTotalArtifact(int totalArtifact) {
        this.totalArtifact = totalArtifact;
    }


    @Override
    public int getSyncStatus() {
        return syncStatus;
    }

    public void setSyncStatus(int syncStatus) {
        this.syncStatus = syncStatus;
    }


    @Override
    public String getUsedSpace() {
        return usedSpace;
    }

    public void setUsedSpace(String usedSpace) {
        this.usedSpace = usedSpace;
    }

    private String usedSpace;

    private String syncDirPath;

    @Override
    public String getSyncDirPath() {
        return syncDirPath;
    }

    public void setSyncDirPath(String syncDirPath) {
        this.syncDirPath = syncDirPath;
    }


    public RepositoryDto() {
    }

    @JsonCreator
    public RepositoryDto(@JsonProperty(value = "id", required = true) String id) {
        this.id = id;
    }

    @Override
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    @Override
    public String getBasedir() {
        return basedir;
    }

    public void setBasedir(String basedir) {
        this.basedir = basedir;
    }

    @Override
    public String getPolicy() {
        return policy;
    }

    public void setPolicy(String policy) {
        this.policy = policy;
    }

    @Override
    public String getStorageProvider() {
        return storageProvider;
    }

    public void setStorageProvider(String storageProvider) {
        this.storageProvider = storageProvider;
    }

    @Override
    public String getLayout() {
        return layout;
    }

    public void setLayout(String layout) {
        this.layout = layout;
    }

    @Override
    public boolean getEnableCustomLayout() {
        return enableCustomLayout;
    }

    public void setEnableCustomLayout(boolean enableCustomLayout) {
        this.enableCustomLayout = enableCustomLayout;
    }

    @Override
    public String getCustomLayout() {
        return customLayout;
    }

    public void setCustomLayout(String customLayout) {
        this.customLayout = customLayout;
    }

    @Override
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    @Override
    public boolean isSecured() {
        return secured;
    }

    public void setSecured(boolean secured) {
        this.secured = secured;
    }

    @Override
    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    @Override
    public boolean isInService() {
        return RepositoryStatusEnum.IN_SERVICE.getStatus().equalsIgnoreCase(getStatus());
    }

    public void putInService() {
        status = RepositoryStatusEnum.IN_SERVICE.getStatus();
    }

    public void putOutOfService() {
        status = RepositoryStatusEnum.OUT_OF_SERVICE.getStatus();
    }

    @Override
    public boolean isTrashEnabled() {
        return trashEnabled;
    }

    public void setTrashEnabled(boolean trashEnabled) {
        this.trashEnabled = trashEnabled;
    }

    @Override
    public boolean isAllowsDeletion() {
        return allowsDeletion;
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
    public boolean isAllowsDirectoryBrowsing() {
        return allowsDirectoryBrowsing;
    }

    @Override
    public boolean isChecksumHeadersEnabled() {
        return checksumHeadersEnabled;
    }

    @Override
    public long getStorageMaxSize() {
        return this.storageMaxSize;
    }

    public void setStorageMaxSize(long storageMaxSize) {
        this.storageMaxSize = storageMaxSize;
    }



    public void setChecksumHeadersEnabled(boolean checksumHeadersEnabled) {
        this.checksumHeadersEnabled = checksumHeadersEnabled;
    }

    public double getStorageThreshold() {
        return storageThreshold;
    }

    public void setStorageThreshold(double storageThreshold) {
        this.storageThreshold = storageThreshold;
    }

    public MutableProxyConfiguration getProxyConfiguration() {
        return proxyConfiguration;
    }

    public void setProxyConfiguration(MutableProxyConfiguration proxyConfiguration) {
        this.proxyConfiguration = proxyConfiguration;
    }

    @Override
    public RemoteRepositoryDto getRemoteRepository() {
        return remoteRepository;
    }

    @Override
    public ProxyConfiguration getProxyConfig() {
        return null;
    }

    public void setRemoteRepository(RemoteRepositoryDto remoteRepository) {
        this.remoteRepository = remoteRepository;
    }

    @Override
    public Set<String> getGroupRepositories() {
        return groupRepositories;
    }

    public void setGroupRepositories(Set<String> groupRepositories) {
        this.groupRepositories = groupRepositories;
    }

    public void addRepositoryToGroup(String repositoryId) {
        groupRepositories.add(repositoryId);
    }

    public void removeRepositoryFromGroup(String repositoryId) {
        groupRepositories.remove(repositoryId);
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
    public Storage getStorage() {
        return storage;
    }

    public void setStorage(StorageDto storage) {
        this.storage = storage;
    }

    @Override
    public String toString() {
        return id;
    }

    public MutableHttpConnectionPool getHttpConnectionPool() {
        return httpConnectionPool;
    }

    public void setHttpConnectionPool(MutableHttpConnectionPool httpConnectionPool) {
        this.httpConnectionPool = httpConnectionPool;
    }

    public List<MutableCustomConfiguration> getCustomConfigurations() {
        return customConfigurations;
    }

    public void setCustomConfigurations(List<MutableCustomConfiguration> customConfigurations) {
        this.customConfigurations = customConfigurations;
    }

    @Override
    public CustomRepositoryConfigurationDto getRepositoryConfiguration() {
        return repositoryConfiguration;
    }

    public void setRepositoryConfiguration(CustomRepositoryConfigurationDto repositoryConfiguration) {
        this.repositoryConfiguration = repositoryConfiguration;
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
    public String getSubLayout() {
        return subLayout;
    }

    public void setSubLayout(String subLayout) {
        this.subLayout = subLayout;
    }

    @Override
    public boolean isType(String compareType) {
        return type.equalsIgnoreCase(compareType);
    }

    public void setAllowsForceDeletion(boolean allowsForceDeletion) {
        this.allowsForceDeletion = allowsForceDeletion;
    }

    public void setAllowsDeployment(boolean allowsDeployment) {
        this.allowsDeployment = allowsDeployment;
    }

    public void setAllowsRedeployment(boolean allowsRedeployment) {
        this.allowsRedeployment = allowsRedeployment;
    }

    public void setAllowsDeletion(boolean allowsDeletion) {
        this.allowsDeletion = allowsDeletion;
    }

    public void setAllowsDirectoryBrowsing(boolean allowsDirectoryBrowsing) {
        this.allowsDirectoryBrowsing = allowsDirectoryBrowsing;
    }

    @Override
    public boolean isHostedRepository() {
        return RepositoryTypeEnum.HOSTED.getType().equals(getType());
    }

    @Override
    public boolean isProxyRepository() {
        return RepositoryTypeEnum.PROXY.getType().equals(getType());
    }

    @Override
    public boolean isGroupRepository() {
        return RepositoryTypeEnum.GROUP.getType().equals(getType());
    }

    public boolean isVirtualRepository() {
        return RepositoryTypeEnum.VIRTUAL.getType().equals(getType());
    }

    @Override
    public long getArtifactMaxSize() {
        return artifactMaxSize;
    }

    public void setArtifactMaxSize(long artifactMaxSize) {
        this.artifactMaxSize = artifactMaxSize;
    }

    @Override
    public Set<String> getArtifactCoordinateValidators() {
        return artifactCoordinateValidators;
    }

    public void setArtifactCoordinateValidators(Set<String> artifactCoordinateValidators) {
        this.artifactCoordinateValidators = artifactCoordinateValidators;
    }

    public boolean isEligibleForCustomConnectionPool() {
        return this.getHttpConnectionPool() != null &&
                this.getRemoteRepository() != null &&
                this.getRemoteRepository().getUrl() != null;
    }

    @Override
    public Set<String> getVulnerabilityWhites() {
        return vulnerabilityWhites;
    }

    public void setVulnerabilityWhites(Set<String> vulnerabilityWhites) {
        this.vulnerabilityWhites = vulnerabilityWhites;
    }

    @Override
    public Set<String> getVulnerabilityBlacks() {
        return vulnerabilityBlacks;
    }

    public void setVulnerabilityBlacks(Set<String> vulnerabilityBlacks) {
        this.vulnerabilityBlacks = vulnerabilityBlacks;
    }

    public void addVulnerabilityWhites(Set<String> whites) {
        this.vulnerabilityWhites.addAll(whites);
    }

    public void removeVulnerabilityWhites(Set<String> whites) {
        this.vulnerabilityWhites.removeAll(whites);
    }

    public void addVulnerabilityBlacks(Set<String> blacks) {
        this.vulnerabilityBlacks.addAll(blacks);
    }

    public void removeVulnerabilityBlacks(Set<String> blacks) {
        this.vulnerabilityBlacks.removeAll(blacks);
    }

    @Override
    public Integer getScope() {
        return scope;
    }

    public void setScope(Integer scope) {
        this.scope = scope;
    }

    @Override
    public boolean isAllowAnonymous() {
        return allowAnonymous;
    }

    @Override
    public UnionRepositoryConfiguration getUnionRepositoryConfig() {
        return null;
    }

    public MutableUnionRepositoryConfiguration getUnionRepositoryConfiguration() {
        return unionRepositoryConfiguration;
    }

    public void setUnionRepositoryConfiguration(MutableUnionRepositoryConfiguration unionRepositoryConfiguration) {
        this.unionRepositoryConfiguration = unionRepositoryConfiguration;
    }

    public void setAllowAnonymous(boolean allowAnonymous) {
        this.allowAnonymous = allowAnonymous;
    }

    public boolean isSyncEnabled() {
        return syncEnabled;
    }

    @Override
    public boolean getSyncJfrog() {
        return this.syncJfrog;
    }

    @Override
    public String getMigrateId() {
        return this.migrateId;
    }


    public void setSyncJfrog(boolean syncJfrog){
        this.syncJfrog=syncJfrog;
    }

    public void setMigrateId(String migrateId){
        this.migrateId=migrateId;
    }



    public void setSyncEnabled(boolean syncEnabled) {
        this.syncEnabled = syncEnabled;
    }

    public void setHealthStatus(Boolean healthStatus){
        this.healthStatus = healthStatus;
    }
}
