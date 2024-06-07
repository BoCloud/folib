package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author leipenghui
 */
@JsonTypeName(PubLayoutProvider.ALIAS)
public class PubRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto {

    private boolean allowsUnpublish = true;

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new PubRepositoryConfigurationData(this);
    }

    public boolean allowsUnpublish() {
        return allowsUnpublish;
    }
}
