package com.folib.configuration.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.layout.providers.CargoLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(CargoLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class CargoRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    public CargoRemoteRepositoryConfiguration(){}
    public CargoRemoteRepositoryConfiguration(CargoRemoteRepositoryConfigurationDto delegate){}
}
