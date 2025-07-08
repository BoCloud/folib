package com.folib.storage.repository.remote;

import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfiguration;

import java.io.Serializable;

/**
 * @author veadan
 */
public interface RemoteRepository
        extends Serializable
{

    String getUrl();

    boolean isDownloadRemoteIndexes();

    boolean isAutoBlocking();

    boolean isChecksumValidation();

    String getUsername();

    String getPassword();

    String getChecksumPolicy();

    Integer getCheckIntervalSeconds();

    boolean allowsDirectoryBrowsing();

    boolean isAutoImportRemoteSSLCertificate();

    CustomRemoteRepositoryConfiguration getCustomConfiguration();
}
