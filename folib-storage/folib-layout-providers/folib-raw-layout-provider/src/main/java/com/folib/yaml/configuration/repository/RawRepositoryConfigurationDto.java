package com.folib.yaml.configuration.repository;

import com.folib.providers.layout.RawLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author Veadan
 * @author Veadan
 */
@JsonTypeName(RawLayoutProvider.ALIAS)
public class RawRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto
{

    @Override
    public CustomRepositoryConfiguration getImmutable()
    {
        return new RawRepositoryConfigurationData(this);
    }
}
