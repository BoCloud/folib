package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
@JsonTypeName(GoLayoutProvider.ALIAS)
public class GoRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new GoRepositoryConfigurationData(this);
    }
}
