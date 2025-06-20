package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.HuggingFaceLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(HuggingFaceLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class HuggingFaceRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    HuggingFaceRemoteRepositoryConfiguration() {
    }

    HuggingFaceRemoteRepositoryConfiguration(HuggingFaceRemoteRepositoryConfigurationDto delegate) {
    }

}
