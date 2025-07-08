package com.folib.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.PhpLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author veadan
 */
@JsonTypeName(PhpLayoutProvider.ALIAS)
public class PhpRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new PhpRepositoryConfigurationData(this);
    }
}
