package com.folib.configuration.remote;

import com.folib.providers.layout.Maven2LayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;


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
