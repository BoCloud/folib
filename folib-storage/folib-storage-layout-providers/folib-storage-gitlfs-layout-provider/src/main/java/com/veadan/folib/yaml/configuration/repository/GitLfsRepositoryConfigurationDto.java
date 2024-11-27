package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

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
