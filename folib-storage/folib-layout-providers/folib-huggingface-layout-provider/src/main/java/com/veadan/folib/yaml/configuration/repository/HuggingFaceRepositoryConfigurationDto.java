package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
@JsonTypeName(HuggingFaceLayoutProvider.ALIAS)
public class HuggingFaceRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {
    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new HuggingFaceRepositoryConfigurationData(this);
    }
}