package com.veadan.folib.storage.repository;

import com.veadan.folib.configuration.MutableProxyConfiguration;
import com.veadan.folib.configuration.ProxyConfiguration;
import com.veadan.folib.providers.storage.FileSystemStorageProvider;
import com.veadan.folib.storage.repository.remote.RemoteRepositoryDto;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.Storage;

import java.io.Serializable;
import java.util.*;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import org.apache.commons.lang3.StringUtils;

/**
 * @author mtodorov
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RepositoryDto
        implements Serializable, Repository
{

    private String id;

    private String basedir;

    private String policy = RepositoryPolicyEnum.MIXED.getPolicy();

    private String storageProvider = FileSystemStorageProvider.ALIAS;

    private String layout;

    private String type = RepositoryTypeEnum.HOSTED.getType();

    private boolean secured;

    private String status = RepositoryStatusEnum.IN_SERVICE.getStatus();

    private long artifactMaxSize;

    private boolean trashEnabled;

    private boolean allowsForceDeletion;

    private boolean allowsDeployment = true;

    private boolean allowsRedeployment = true;

    private boolean allowsDelete = true;

    private boolean allowsDirectoryBrowsing = true;

    private boolean checksumHeadersEnabled;

    /**
     * The per-repository proxy settings that override the overall global proxy settings.
     */
    private MutableProxyConfiguration proxyConfiguration;

    private RemoteRepositoryDto remoteRepository;

    private MutableHttpConnectionPool httpConnectionPool;

    private List<MutableCustomConfiguration> customConfigurations = new ArrayList<>();

    private CustomRepositoryConfigurationDto repositoryConfiguration;

    private Set<String> groupRepositories = new LinkedHashSet<>();

    private Set<String> artifactCoordinateValidators = new LinkedHashSet<>();

    @JsonIgnore
    private StorageDto storage;


    public RepositoryDto()
    {
    }

    @JsonCreator
    public RepositoryDto(@JsonProperty(value = "id", required = true) String id)
    {
        this.id = id;
    }

    public String getId()
    {
        return id;
    }

    public void setId(String id)
    {
        this.id = id;
    }

    public String getBasedir()
    {
        return basedir;
    }

    public void setBasedir(String basedir)
    {
        this.basedir = basedir;
    }

    public String getPolicy()
    {
        return policy;
    }

    public void setPolicy(String policy)
    {
        this.policy = policy;
    }

    public String getStorageProvider()
    {
        return storageProvider;
    }

    public void setStorageProvider(String storageProvider)
    {
        this.storageProvider = storageProvider;
    }

    public String getLayout()
    {
        return layout;
    }

    public void setLayout(String layout)
    {
        this.layout = layout;
    }

    public String getType()
    {
        return type;
    }

    public void setType(String type)
    {
        this.type = type;
    }

    public boolean isSecured()
    {
        return secured;
    }

    public void setSecured(boolean secured)
    {
        this.secured = secured;
    }

    public String getStatus()
    {
        return status;
    }

    public void setStatus(String status)
    {
        this.status = status;
    }

    public boolean isInService()
    {
        return RepositoryStatusEnum.IN_SERVICE.getStatus().equalsIgnoreCase(getStatus());
    }

    public void putInService()
    {
        status = RepositoryStatusEnum.IN_SERVICE.getStatus();
    }

    public void putOutOfService()
    {
        status = RepositoryStatusEnum.OUT_OF_SERVICE.getStatus();
    }

    public boolean isTrashEnabled()
    {
        return trashEnabled;
    }

    public void setTrashEnabled(boolean trashEnabled)
    {
        this.trashEnabled = trashEnabled;
    }

    public boolean allowsDeletion()
    {
        return allowsDelete;
    }

    public boolean allowsForceDeletion()
    {
        return allowsForceDeletion;
    }

    public boolean allowsDeployment()
    {
        return allowsDeployment;
    }

    public boolean allowsRedeployment()
    {
        return allowsRedeployment;
    }

    public boolean allowsDirectoryBrowsing()
    {
        return allowsDirectoryBrowsing;
    }

    public boolean isChecksumHeadersEnabled()
    {
        return checksumHeadersEnabled;
    }

    public void setChecksumHeadersEnabled(boolean checksumHeadersEnabled)
    {
        this.checksumHeadersEnabled = checksumHeadersEnabled;
    }

    public MutableProxyConfiguration getProxyConfiguration()
    {
        return proxyConfiguration;
    }

    public void setProxyConfiguration(MutableProxyConfiguration proxyConfiguration)
    {
        this.proxyConfiguration = proxyConfiguration;
    }

    public RemoteRepositoryDto getRemoteRepository()
    {
        return remoteRepository;
    }

    @Override
    public ProxyConfiguration getProxyConfig() {
        return null;
    }

    public void setRemoteRepository(RemoteRepositoryDto remoteRepository)
    {
        this.remoteRepository = remoteRepository;
    }

    public Set<String> getGroupRepositories()
    {
        return groupRepositories;
    }

    public void setGroupRepositories(Set<String> groupRepositories)
    {
        this.groupRepositories = groupRepositories;
    }

    public void addRepositoryToGroup(String repositoryId)
    {
        groupRepositories.add(repositoryId);
    }

    public void removeRepositoryFromGroup(String repositoryId)
    {
        groupRepositories.remove(repositoryId);
    }

    public boolean acceptsSnapshots()
    {
        return RepositoryPolicyEnum.ofPolicy(getPolicy()).acceptsSnapshots();
    }

    public boolean acceptsReleases()
    {
        return RepositoryPolicyEnum.ofPolicy(getPolicy()).acceptsReleases();
    }

    public Storage getStorage()
    {
        return storage;
    }

    public void setStorage(StorageDto storage)
    {
        this.storage = storage;
    }

    @Override
    public String toString()
    {
        return id;
    }

    public MutableHttpConnectionPool getHttpConnectionPool()
    {
        return httpConnectionPool;
    }

    public void setHttpConnectionPool(MutableHttpConnectionPool httpConnectionPool)
    {
        this.httpConnectionPool = httpConnectionPool;
    }

    public List<MutableCustomConfiguration> getCustomConfigurations()
    {
        return customConfigurations;
    }

    public void setCustomConfigurations(List<MutableCustomConfiguration> customConfigurations)
    {
        this.customConfigurations = customConfigurations;
    }

    public CustomRepositoryConfigurationDto getRepositoryConfiguration()
    {
        return repositoryConfiguration;
    }

    public void setRepositoryConfiguration(CustomRepositoryConfigurationDto repositoryConfiguration)
    {
        this.repositoryConfiguration = repositoryConfiguration;
    }

    @Override
    @JsonIgnore
    public String getStorageIdAndRepositoryId()
    {
        StringJoiner storageAndRepositoryId = new StringJoiner(":");

        if (StringUtils.isNotBlank(getStorage().getId()))
        {
            storageAndRepositoryId.add(getStorage().getId());
        }

        if (StringUtils.isNotBlank(getId()))
        {
            storageAndRepositoryId.add(getId());
        }

        return storageAndRepositoryId.toString();
    }

    @Override
    public boolean isType(String compareType)
    {
        return type.equalsIgnoreCase(compareType);
    }

    public void setAllowsForceDeletion(boolean allowsForceDeletion)
    {
        this.allowsForceDeletion = allowsForceDeletion;
    }

    public void setAllowsDeployment(boolean allowsDeployment)
    {
        this.allowsDeployment = allowsDeployment;
    }

    public void setAllowsRedeployment(boolean allowsRedeployment)
    {
        this.allowsRedeployment = allowsRedeployment;
    }

    public void setAllowsDelete(boolean allowsDelete)
    {
        this.allowsDelete = allowsDelete;
    }

    public void setAllowsDirectoryBrowsing(boolean allowsDirectoryBrowsing)
    {
        this.allowsDirectoryBrowsing = allowsDirectoryBrowsing;
    }

    public boolean isHostedRepository()
    {
        return RepositoryTypeEnum.HOSTED.getType().equals(getType());
    }

    public boolean isProxyRepository()
    {
        return RepositoryTypeEnum.PROXY.getType().equals(getType());
    }

    public boolean isGroupRepository()
    {
        return RepositoryTypeEnum.GROUP.getType().equals(getType());
    }

    public boolean isVirtualRepository()
    {
        return RepositoryTypeEnum.VIRTUAL.getType().equals(getType());
    }

    public long getArtifactMaxSize()
    {
        return artifactMaxSize;
    }

    public void setArtifactMaxSize(long artifactMaxSize)
    {
        this.artifactMaxSize = artifactMaxSize;
    }

    public Set<String> getArtifactCoordinateValidators()
    {
        return artifactCoordinateValidators;
    }

    public void setArtifactCoordinateValidators(Set<String> artifactCoordinateValidators)
    {
        this.artifactCoordinateValidators = artifactCoordinateValidators;
    }

    public boolean isEligibleForCustomConnectionPool()
    {
        return this.getHttpConnectionPool() != null &&
               this.getRemoteRepository() != null &&
               this.getRemoteRepository().getUrl() != null;
    }

}
