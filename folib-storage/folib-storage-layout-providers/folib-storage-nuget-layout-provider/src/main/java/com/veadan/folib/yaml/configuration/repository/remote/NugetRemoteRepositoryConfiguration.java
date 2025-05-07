package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.NugetLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;



import com.fasterxml.jackson.annotation.JsonTypeName;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

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
