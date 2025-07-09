package com.folib.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.CocoapodsLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

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
