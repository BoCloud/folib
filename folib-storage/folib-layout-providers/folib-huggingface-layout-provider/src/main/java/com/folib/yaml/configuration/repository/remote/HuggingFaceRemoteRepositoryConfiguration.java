package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.HuggingFaceLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
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
