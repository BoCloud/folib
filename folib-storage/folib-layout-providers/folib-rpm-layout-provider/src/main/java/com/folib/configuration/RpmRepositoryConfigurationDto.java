package com.folib.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.RpmLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;
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
