package com.folib.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.PhpLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
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
