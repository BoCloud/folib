package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.NugetLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName(NugetLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class NugetRemoteRepositoryConfiguration
        extends CustomRemoteRepositoryConfigurationData
{

    NugetRemoteRepositoryConfiguration()
    {
    }

    NugetRemoteRepositoryConfiguration(NugetRemoteRepositoryConfigurationDto delegate)
    {
    }

}
