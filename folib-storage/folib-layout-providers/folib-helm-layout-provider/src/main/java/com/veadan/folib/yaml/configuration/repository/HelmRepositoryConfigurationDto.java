package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.HelmLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

@JsonTypeName(HelmLayoutProvider.ALIAS)
public class HelmRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {
    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new HelmRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish() {
        return allowsUnpublish;
    }
}
