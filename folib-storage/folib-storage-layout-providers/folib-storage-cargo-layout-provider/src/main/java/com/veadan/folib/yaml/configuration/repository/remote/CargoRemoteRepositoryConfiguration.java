package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.layout.providers.CargoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(CargoLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class CargoRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    public CargoRemoteRepositoryConfiguration(){}
    public CargoRemoteRepositoryConfiguration(CargoRemoteRepositoryConfigurationDto delegate){}
}
