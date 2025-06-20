package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(PubLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class PubRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    PubRemoteRepositoryConfiguration() {
    }

    PubRemoteRepositoryConfiguration(PubRemoteRepositoryConfigurationDto delegate) {
    }
}
