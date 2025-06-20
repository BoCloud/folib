package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.layout.providers.CargoLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

@JsonTypeName(CargoLayoutProvider.ALIAS)
public class CargoRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto {
    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new CargoRemoteRepositoryConfiguration();
    }
}
