package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.GoLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
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
