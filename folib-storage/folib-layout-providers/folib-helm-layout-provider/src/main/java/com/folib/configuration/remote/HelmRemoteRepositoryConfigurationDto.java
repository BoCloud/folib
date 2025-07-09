package com.folib.configuration.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.HelmLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(HelmLayoutProvider.ALIAS)
public class HelmRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new HelmRemoteRepositoryConfiguration();
    }

}