package com.veadan.folib.converters.configuration;

import com.veadan.folib.dto.configuration.RemoteRepositoryDto;

import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 */
public enum RemoteRepositoryFormConverter
        implements Converter<RemoteRepositoryDto, com.veadan.folib.storage.repository.remote.RemoteRepositoryDto>
{

    INSTANCE;

    @Override
    public com.veadan.folib.storage.repository.remote.RemoteRepositoryDto convert(final RemoteRepositoryDto source)
    {
        com.veadan.folib.storage.repository.remote.RemoteRepositoryDto result = new com.veadan.folib.storage.repository.remote.RemoteRepositoryDto();
        result.setUrl(source.getUrl());
        result.setDownloadRemoteIndexes(source.isDownloadRemoteIndexes());
        result.setAutoBlocking(source.isAutoBlocking());
        result.setChecksumValidation(source.isChecksumValidation());
        result.setUsername(source.getUsername());
        result.setPassword(source.getPassword());
        result.setChecksumPolicy(source.getChecksumPolicy());
        result.setCheckIntervalSeconds(source.getCheckIntervalSeconds());
        result.setAllowsDirectoryBrowsing(source.isAllowsDirectoryBrowsing());
        result.setAutoImportRemoteSSLCertificate(source.isAutoImportRemoteSSLCertificate());
        return result;
    }
}
