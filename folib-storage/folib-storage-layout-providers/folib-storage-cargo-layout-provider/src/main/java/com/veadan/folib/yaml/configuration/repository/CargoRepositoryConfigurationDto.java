package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.layout.providers.CargoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

@JsonTypeName(CargoLayoutProvider.ALIAS)
public class CargoRepositoryConfigurationDto extends CustomRepositoryConfigurationDto {

    @Override
    public CustomRepositoryConfiguration getImmutable() {
        return new CargoRepositoryConfigurationData(this);
    }
}
