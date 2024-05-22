package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

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
