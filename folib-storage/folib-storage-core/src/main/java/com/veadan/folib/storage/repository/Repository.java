package com.veadan.folib.storage.repository;

import java.util.Set;

import com.veadan.folib.configuration.ProxyConfiguration;
import com.veadan.folib.storage.repository.remote.RemoteRepository;
import com.veadan.folib.yaml.repository.RepositoryConfiguration;
import com.veadan.folib.storage.Storage;

public interface Repository
{

    String getId();

    String getBasedir();

    String getPolicy();

    String getStorageProvider();

    String getLayout();

    String getType();

    boolean isSecured();

    String getStatus();

    long getArtifactMaxSize();

    boolean isTrashEnabled();

    boolean allowsForceDeletion();

    boolean allowsDeployment();

    boolean allowsRedeployment();

    boolean allowsDeletion();

    boolean allowsDirectoryBrowsing();

    boolean isChecksumHeadersEnabled();

    Set<String> getGroupRepositories();

    Set<String> getArtifactCoordinateValidators();

    Storage getStorage();

    boolean isHostedRepository();

    boolean isProxyRepository();

    boolean isGroupRepository();

    boolean isInService();

    boolean acceptsSnapshots();

    boolean acceptsReleases();
    
    RepositoryConfiguration getRepositoryConfiguration();

    String getStorageIdAndRepositoryId();

    boolean isType(String type);

    RemoteRepository getRemoteRepository();

    ProxyConfiguration getProxyConfig();

    Set<String> getVulnerabilityWhites();

    Set<String> getVulnerabilityBlacks();

}
