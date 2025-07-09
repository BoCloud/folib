package com.folib.configuration.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.CocoapodsLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(CocoapodsLayoutProvider.ALIAS)
public class CocoapodsRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new CocoapodsRemoteRepositoryConfiguration();
    }

}