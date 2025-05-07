package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(GitLfsLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class GitLfsRemoteRepositoryConfiguration
        extends CustomRemoteRepositoryConfigurationData
{

    GitLfsRemoteRepositoryConfiguration()
    {
    }

    GitLfsRemoteRepositoryConfiguration(GitLfsRemoteRepositoryConfigurationDto delegate)
    {
    }

}
