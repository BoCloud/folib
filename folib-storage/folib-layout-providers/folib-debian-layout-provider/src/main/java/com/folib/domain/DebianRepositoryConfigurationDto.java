package com.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author veadan
 */
@JsonTypeName(DebianLayoutProvider.ALIAS)
public class DebianRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {
    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new DebianRepositoryConfigurationData(this);
    }
}
