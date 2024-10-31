package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.DebianLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author huayanjun
 */
@JsonTypeName(DebianLayoutProvider.ALIAS)
public class DebianRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {
    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new DebianRepositoryConfigurationData(this);
    }
}
