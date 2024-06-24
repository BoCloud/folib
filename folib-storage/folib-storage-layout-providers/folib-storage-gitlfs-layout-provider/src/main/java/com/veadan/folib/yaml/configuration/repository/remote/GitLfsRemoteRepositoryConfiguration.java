package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

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
