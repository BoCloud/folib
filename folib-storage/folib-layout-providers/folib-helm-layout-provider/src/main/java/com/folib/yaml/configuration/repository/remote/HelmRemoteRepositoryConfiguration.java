package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.HelmLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(HelmLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class HelmRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    HelmRemoteRepositoryConfiguration() {
    }

    HelmRemoteRepositoryConfiguration(HelmRemoteRepositoryConfigurationDto delegate) {
    }

}