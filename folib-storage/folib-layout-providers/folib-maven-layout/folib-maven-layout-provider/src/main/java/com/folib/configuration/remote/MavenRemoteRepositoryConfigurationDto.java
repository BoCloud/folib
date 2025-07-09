package com.folib.configuration.remote;

import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author Veadan
 */
@JsonTypeName(Maven2LayoutProvider.ALIAS)
public class MavenRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto
{

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable()
    {
        return new MavenRemoteRepositoryConfiguration();
    }

}
