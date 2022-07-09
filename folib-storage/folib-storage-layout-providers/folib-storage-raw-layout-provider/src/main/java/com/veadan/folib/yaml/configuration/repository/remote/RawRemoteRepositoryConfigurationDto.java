package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

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
