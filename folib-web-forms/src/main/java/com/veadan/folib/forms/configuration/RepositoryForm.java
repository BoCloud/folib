package com.veadan.folib.forms.configuration;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.veadan.folib.storage.repository.RepositoryPolicyEnum;
import com.veadan.folib.storage.repository.RepositoryStatusEnum;
import com.veadan.folib.storage.repository.RepositoryTypeEnum;
import com.veadan.folib.validation.configuration.DescribableEnumValue;
import com.veadan.folib.validation.configuration.LayoutProviderValue;
import com.veadan.folib.validation.configuration.StorageProviderValue;

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

    @NotEmpty(message = "A type must be specified.")
    @DescribableEnumValue(message = "The type value is invalid.", type = RepositoryTypeEnum.class)
    private String type;

    private boolean secured;

    @NotEmpty(message = "A status must be specified.")
    @DescribableEnumValue(message = "The status value is invalid.", type = RepositoryStatusEnum.class)
    private String status;

    private String storageId;

    private long artifactMaxSize;

    private boolean trashEnabled = true;

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

    private Set<String> artifactCoordinateValidators;

    /**
     * 白名单列表
     */
    @NotEmpty(message = "请填写白名单", groups = {RepositoryForm.WhiteGroup.class})
    private Set<String> vulnerabilityWhites;
    /**
     * 黑名单列表
     */
    @NotEmpty(message = "请填写黑名单", groups = {RepositoryForm.BlackGroup.class})
    private Set<String> vulnerabilityBlacks;

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

    public boolean isTrashEnabled() {
        return trashEnabled;
    }

    public void setTrashEnabled(final boolean trashEnabled) {
        this.trashEnabled = trashEnabled;
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
