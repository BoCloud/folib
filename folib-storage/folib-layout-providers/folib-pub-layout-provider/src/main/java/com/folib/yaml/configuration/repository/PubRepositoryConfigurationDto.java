package com.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.PubLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author veadan
 */
@JsonTypeName(PubLayoutProvider.ALIAS)
public class PubRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new PubRepositoryConfigurationData(this);
    }
}
