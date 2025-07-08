package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(HuggingFaceLayoutProvider.ALIAS)
public class HuggingFaceRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {
    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new HuggingFaceRemoteRepositoryConfiguration();
    }

}
