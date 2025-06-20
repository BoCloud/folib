package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;


import com.fasterxml.jackson.annotation.JsonTypeName;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(Maven2LayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class MavenRemoteRepositoryConfiguration
        extends CustomRemoteRepositoryConfigurationData
{

    MavenRemoteRepositoryConfiguration()
    {
    }

    MavenRemoteRepositoryConfiguration(MavenRemoteRepositoryConfigurationDto delegate)
    {
    }

}
