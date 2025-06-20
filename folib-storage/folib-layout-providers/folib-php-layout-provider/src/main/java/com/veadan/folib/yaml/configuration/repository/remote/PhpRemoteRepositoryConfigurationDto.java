package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PhpLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author leipenghui
 */
@JsonTypeName(PhpLayoutProvider.ALIAS)
public class PhpRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new PhpRemoteRepositoryConfiguration();
    }

}
