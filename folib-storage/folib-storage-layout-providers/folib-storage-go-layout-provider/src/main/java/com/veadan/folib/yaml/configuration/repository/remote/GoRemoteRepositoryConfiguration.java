package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * @author pengYongQiang
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
