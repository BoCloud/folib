package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;


import com.fasterxml.jackson.annotation.JsonTypeName;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(RawLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class RawRemoteRepositoryConfiguration
        extends CustomRemoteRepositoryConfigurationData
{

    RawRemoteRepositoryConfiguration()
    {
    }

    RawRemoteRepositoryConfiguration(RawRemoteRepositoryConfigurationDto delegate)
    {
    }

}
