package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(HuggingFaceLayoutProvider.ALIAS)
public class HuggingFaceRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {
    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new HuggingFaceRemoteRepositoryConfiguration();
    }

}
