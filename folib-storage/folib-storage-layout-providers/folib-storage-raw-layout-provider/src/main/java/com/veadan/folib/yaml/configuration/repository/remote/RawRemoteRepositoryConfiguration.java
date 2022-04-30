package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.fasterxml.jackson.annotation.JsonTypeName;

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
