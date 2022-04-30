package com.veadan.folib.services;

import com.veadan.folib.client.MutableRemoteRepositoryRetryArtifactDownloadConfiguration;
import com.veadan.folib.configuration.Configuration;
import com.veadan.folib.configuration.MutableConfiguration;
import com.veadan.folib.configuration.MutableProxyConfiguration;
import com.veadan.folib.configuration.MutableSmtpConfiguration;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.RepositoryDto;
import com.veadan.folib.storage.routing.MutableRoutingRule;
import com.veadan.folib.storage.routing.MutableRoutingRules;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

/**
 * @author mtodorov
 */
public interface ConfigurationManagementService
{

    MutableConfiguration getMutableConfigurationClone();

    Configuration getConfiguration();

    void setConfiguration(MutableConfiguration configuration) throws IOException;

    void setInstanceName(String instanceName) throws IOException;

    void setBaseUrl(String baseUrl) throws IOException;

    void setPort(int port) throws IOException;

    void setProxyConfiguration(String storageId,
                               String repositoryId,
                               MutableProxyConfiguration proxyConfiguration) throws IOException;

    void saveStorage(StorageDto storage) throws IOException;
    
    void addStorageIfNotExists(StorageDto storage) throws IOException;

    void removeStorage(String storageId) throws IOException;

    void saveRepository(String storageId,
                        RepositoryDto repository) throws IOException;

    void removeRepositoryFromAssociatedGroups(String storageId,
                                              String repositoryId) throws IOException;

    void removeRepository(String storageId,
                          String repositoryId) throws IOException;

    void setProxyRepositoryMaxConnections(String storageId,
                                          String repositoryId,
                                          int numberOfConnections) throws IOException;

    MutableRoutingRules getRoutingRules();

    MutableRoutingRule getRoutingRule(UUID uuid);

    boolean updateRoutingRule(UUID uuid,
                              MutableRoutingRule routingRule) throws IOException;

    boolean addRoutingRule(MutableRoutingRule routingRule) throws IOException;

    boolean removeRoutingRule(UUID uuid) throws IOException;

    void addRepositoryToGroup(String storageId,
                              String repositoryId,
                              String repositoryGroupMemberId) throws IOException;

    void setRepositoryArtifactCoordinateValidators() throws IOException;

    void putInService(String storageId,
                      String repositoryId) throws IOException;

    void putOutOfService(String storageId,
                         String repositoryId) throws IOException;

    void setArtifactMaxSize(String storageId,
                            String repositoryId,
                            long value) throws IOException;

    void set(MutableRemoteRepositoryRetryArtifactDownloadConfiguration remoteRepositoryRetryArtifactDownloadConfiguration) throws IOException;

    void addRepositoryArtifactCoordinateValidator(String storageId,
                                                  String repositoryId,
                                                  String alias) throws IOException;

    boolean removeRepositoryArtifactCoordinateValidator(String storageId,
                                                        String repositoryId,
                                                        String alias) throws IOException;

    void setCorsAllowedOrigins(List<String> allowedOrigins) throws IOException;

    void setSmtpSettings(MutableSmtpConfiguration smtpConfiguration) throws IOException;

}
