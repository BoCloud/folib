package com.folib.yaml.configuration.repository.remote;

import com.folib.providers.layout.NugetLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;



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
