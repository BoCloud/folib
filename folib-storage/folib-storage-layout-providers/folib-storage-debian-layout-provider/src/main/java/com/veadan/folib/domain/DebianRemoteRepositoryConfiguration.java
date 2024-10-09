package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.DebianLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(DebianLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class DebianRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    DebianRemoteRepositoryConfiguration() {
    }

    DebianRemoteRepositoryConfiguration(DebianRemoteRepositoryConfigurationDto delegate) {
    }
}
