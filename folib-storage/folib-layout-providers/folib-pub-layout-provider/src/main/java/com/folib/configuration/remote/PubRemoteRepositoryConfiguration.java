package com.folib.configuration.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.PubLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
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
