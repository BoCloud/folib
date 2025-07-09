package com.folib.configuration.remote;


import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.ConanLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
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