package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.CocoapodsLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author Veadan
 * @author Veadan
 */
@JsonTypeName(CocoapodsLayoutProvider.ALIAS)
public class CocoapodsRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto
{

    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable()
    {
        return new CocoapodsRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish()
    {
        return allowsUnpublish;
    }
}
