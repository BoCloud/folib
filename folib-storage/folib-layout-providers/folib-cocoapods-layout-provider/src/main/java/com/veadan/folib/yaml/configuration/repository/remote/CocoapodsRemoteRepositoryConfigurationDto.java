package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.CocoapodsLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(CocoapodsLayoutProvider.ALIAS)
public class CocoapodsRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new CocoapodsRemoteRepositoryConfiguration();
    }

}