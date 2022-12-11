package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.HelmLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(HelmLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class HelmRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    HelmRemoteRepositoryConfiguration() {
    }

    HelmRemoteRepositoryConfiguration(HelmRemoteRepositoryConfigurationDto delegate) {
    }

}