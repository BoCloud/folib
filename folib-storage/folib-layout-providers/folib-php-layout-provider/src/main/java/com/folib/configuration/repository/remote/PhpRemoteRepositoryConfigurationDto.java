package com.folib.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.PhpLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author veadan
 */
@JsonTypeName(PhpLayoutProvider.ALIAS)
public class PhpRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new PhpRemoteRepositoryConfiguration();
    }

}
