package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.layout.providers.CargoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(CargoLayoutProvider.ALIAS)
public class CargoRepositoryConfigurationData extends CustomRepositoryConfiguration {

    public CargoRepositoryConfigurationData() {
    }

    public CargoRepositoryConfigurationData(final CargoRepositoryConfigurationDto delegate) {
        // maybe one day I'll have some implementation here :)
    }

}
