package com.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.layout.providers.CargoLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import javax.annotation.concurrent.Immutable;


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
