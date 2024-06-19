package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author leipenghui
 */
@JsonTypeName(PubLayoutProvider.ALIAS)
public class PubRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new PubRemoteRepositoryConfiguration();
    }

}
