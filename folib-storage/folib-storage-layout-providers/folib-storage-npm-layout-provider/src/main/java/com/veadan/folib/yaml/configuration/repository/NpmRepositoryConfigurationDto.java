package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.NpmLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author Veadan
 * @author Pablo Tirado
 */
@JsonTypeName(NpmLayoutProvider.ALIAS)
public class NpmRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto
{

    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable()
    {
        return new NpmRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish()
    {
        return allowsUnpublish;
    }
}
