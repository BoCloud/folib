package com.folib.configuration.remote;

import com.folib.providers.RawLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author Veadan
 */
@JsonTypeName(RawLayoutProvider.ALIAS)
public class RawRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto
{

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable()
    {
        return new RawRemoteRepositoryConfiguration();
    }

}
