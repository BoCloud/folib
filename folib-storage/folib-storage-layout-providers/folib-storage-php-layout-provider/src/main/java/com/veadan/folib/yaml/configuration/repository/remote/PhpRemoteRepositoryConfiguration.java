package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PhpLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

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
