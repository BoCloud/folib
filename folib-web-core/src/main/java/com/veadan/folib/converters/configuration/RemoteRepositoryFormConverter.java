package com.veadan.folib.converters.configuration;

import com.veadan.folib.forms.configuration.RemoteRepositoryForm;
import com.veadan.folib.storage.repository.remote.RemoteRepositoryDto;

import org.springframework.core.convert.converter.Converter;

/**
 * @author veadan
 */
public enum RemoteRepositoryFormConverter
        implements Converter<RemoteRepositoryForm, RemoteRepositoryDto>
{

    INSTANCE;

    @Override
    public RemoteRepositoryDto convert(final RemoteRepositoryForm source)
    {
        RemoteRepositoryDto result = new RemoteRepositoryDto();
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
