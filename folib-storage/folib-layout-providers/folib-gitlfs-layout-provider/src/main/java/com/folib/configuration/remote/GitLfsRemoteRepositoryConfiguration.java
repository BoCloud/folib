package com.folib.configuration.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.GitLfsLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
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
