package com.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.layout.providers.CargoLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

@JsonTypeName(CargoLayoutProvider.ALIAS)
public class CargoRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new CargoRepositoryConfigurationData(this);
    }
}
