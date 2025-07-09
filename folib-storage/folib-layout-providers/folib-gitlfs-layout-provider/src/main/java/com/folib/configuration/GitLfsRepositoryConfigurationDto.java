package com.folib.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.GitLfsLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import com.folib.yaml.repository.CustomRepositoryConfigurationDto;

/**
 * @author Veadan
 * @author Veadan
 */
@JsonTypeName(GitLfsLayoutProvider.ALIAS)
public class GitLfsRepositoryConfigurationDto extends CustomRepositoryConfigurationDto
{

    @Override
    public CustomRepositoryConfiguration getImmutable()
    {
        return new GitLfsRepositoryConfigurationData(this);
    }
}
