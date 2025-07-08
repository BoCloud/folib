package com.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author veadan
 */
@JsonTypeName(DebianLayoutProvider.ALIAS)
public class DebianRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new DebianRemoteRepositoryConfiguration();
    }

}
