package com.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.GitLfsLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author Veadan
 */
@JsonTypeName(GitLfsLayoutProvider.ALIAS)
public class GitLfsRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto
{

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable()
    {
        return new GitLfsRemoteRepositoryConfiguration();
    }

}
