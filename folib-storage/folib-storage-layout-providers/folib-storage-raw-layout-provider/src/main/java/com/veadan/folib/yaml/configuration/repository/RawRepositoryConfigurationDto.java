package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

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
