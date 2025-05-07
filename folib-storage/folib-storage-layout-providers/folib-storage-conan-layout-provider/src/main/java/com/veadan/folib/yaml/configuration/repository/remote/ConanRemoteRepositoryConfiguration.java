package com.veadan.folib.yaml.configuration.repository.remote;


import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.ConanLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(ConanLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class ConanRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    ConanRemoteRepositoryConfiguration() {
    }

    ConanRemoteRepositoryConfiguration(ConanRemoteRepositoryConfigurationDto delegate) {
    }

}