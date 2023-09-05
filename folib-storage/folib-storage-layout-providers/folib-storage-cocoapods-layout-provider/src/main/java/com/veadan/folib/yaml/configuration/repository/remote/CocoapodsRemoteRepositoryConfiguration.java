package com.veadan.folib.yaml.configuration.repository.remote;


import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.CocoapodsLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(CocoapodsLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class CocoapodsRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    CocoapodsRemoteRepositoryConfiguration() {
    }

    CocoapodsRemoteRepositoryConfiguration(CocoapodsRemoteRepositoryConfigurationDto delegate) {
    }

}