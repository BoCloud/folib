package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.fasterxml.jackson.annotation.JsonTypeName;

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
