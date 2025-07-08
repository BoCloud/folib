package com.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

@JsonTypeName(HuggingFaceLayoutProvider.ALIAS)
public class HuggingFaceRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {
    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new HuggingFaceRepositoryConfigurationData(this);
    }
}