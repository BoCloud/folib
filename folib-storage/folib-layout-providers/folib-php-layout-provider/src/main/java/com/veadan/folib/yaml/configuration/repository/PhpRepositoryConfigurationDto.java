package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PhpLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

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
