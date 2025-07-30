package com.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.CondaLayoutProvider;

import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;


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
