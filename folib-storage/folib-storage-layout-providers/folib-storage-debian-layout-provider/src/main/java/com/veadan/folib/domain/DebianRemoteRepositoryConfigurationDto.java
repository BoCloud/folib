package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.DebianLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author huayanjun
 */
@JsonTypeName(DebianLayoutProvider.ALIAS)
public class DebianRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto {

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new DebianRemoteRepositoryConfiguration();
    }

}
