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
