package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@JsonTypeName(GoLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class GoRemoteRepositoryConfiguration
        extends CustomRemoteRepositoryConfigurationData {

    GoRemoteRepositoryConfiguration() {
    }

    GoRemoteRepositoryConfiguration(GoRemoteRepositoryConfigurationDto delegate) {
    }

}
