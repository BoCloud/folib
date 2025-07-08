package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.PhpLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


/**
 * @author leipenghui
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
