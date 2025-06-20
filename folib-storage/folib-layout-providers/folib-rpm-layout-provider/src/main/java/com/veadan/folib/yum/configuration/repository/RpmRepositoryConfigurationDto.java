package com.veadan.folib.yum.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.RpmLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
@JsonTypeName(RpmLayoutProvider.ALIAS)
public class RpmRepositoryConfigurationDto extends CustomRepositoryConfigurationDto
{

    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable()
    {
        return new RpmRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish()
    {
        return allowsUnpublish;
    }
}
