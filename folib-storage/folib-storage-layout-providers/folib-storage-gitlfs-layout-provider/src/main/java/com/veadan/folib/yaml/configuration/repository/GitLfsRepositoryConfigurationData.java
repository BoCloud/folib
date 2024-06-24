package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GitLfsLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(GitLfsLayoutProvider.ALIAS)
public class GitLfsRepositoryConfigurationData
        extends CustomRepositoryConfiguration
{

    public GitLfsRepositoryConfigurationData()
    {

    }

    public GitLfsRepositoryConfigurationData(final GitLfsRepositoryConfigurationDto delegate)
    {
        // maybe one day I'll have some implementation here :)
    }

}
