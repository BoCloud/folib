package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

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
