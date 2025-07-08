package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.layout.providers.CargoLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(CargoLayoutProvider.ALIAS)
public class CargoRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {
    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new CargoRemoteRepositoryConfiguration();
    }
}
