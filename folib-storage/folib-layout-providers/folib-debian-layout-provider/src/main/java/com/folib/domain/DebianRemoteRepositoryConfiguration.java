package com.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.DebianLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(DebianLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class DebianRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    DebianRemoteRepositoryConfiguration() {
    }

    DebianRemoteRepositoryConfiguration(DebianRemoteRepositoryConfigurationDto delegate) {
    }
}
