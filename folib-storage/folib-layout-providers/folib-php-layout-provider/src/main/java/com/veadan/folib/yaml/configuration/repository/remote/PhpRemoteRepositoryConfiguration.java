package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PhpLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


/**
 * @author veadan
 */
@JsonTypeName(PhpLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class PhpRemoteRepositoryConfiguration
        extends CustomRemoteRepositoryConfigurationData {

    PhpRemoteRepositoryConfiguration() {
    }

    PhpRemoteRepositoryConfiguration(PhpRemoteRepositoryConfigurationDto delegate) {
    }

}
