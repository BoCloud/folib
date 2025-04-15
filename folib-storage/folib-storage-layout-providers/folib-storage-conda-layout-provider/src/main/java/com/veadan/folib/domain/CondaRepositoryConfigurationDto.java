package com.veadan.folib.domain;

import com.veadan.folib.providers.layout.CondaLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;


@JsonTypeName(CondaLayoutProvider.ALIAS)
public class CondaRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {

    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new CondaRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish() {
        return allowsUnpublish;
    }

}
