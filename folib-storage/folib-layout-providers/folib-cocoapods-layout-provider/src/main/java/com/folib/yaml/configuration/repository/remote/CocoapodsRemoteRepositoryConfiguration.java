package com.folib.yaml.configuration.repository.remote;


import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.CocoapodsLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(CocoapodsLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class CocoapodsRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    CocoapodsRemoteRepositoryConfiguration() {
    }

    CocoapodsRemoteRepositoryConfiguration(CocoapodsRemoteRepositoryConfigurationDto delegate) {
    }

}