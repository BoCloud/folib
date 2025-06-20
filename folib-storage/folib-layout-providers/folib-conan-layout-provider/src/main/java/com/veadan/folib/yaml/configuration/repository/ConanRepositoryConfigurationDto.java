package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.ConanLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

@JsonTypeName(ConanLayoutProvider.ALIAS)
public class ConanRepositoryConfigurationDto  extends CustomRepositoryConfigurationDto {
    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new ConanRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish() {
        return allowsUnpublish;
    }
}
