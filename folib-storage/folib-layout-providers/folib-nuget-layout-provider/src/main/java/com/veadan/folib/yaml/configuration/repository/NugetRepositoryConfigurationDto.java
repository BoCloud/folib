package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.NugetLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.yaml.repository.CustomRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author Veadan
 * @author Veadan
 */
@JsonTypeName(NugetLayoutProvider.ALIAS)
public class NugetRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto implements NugetRepositoryConfiguration
{
    private String feedVersion = "2.0";

    private Integer remoteFeedPageSize;

    public String getFeedVersion()
    {
        return feedVersion;
    }

    public void setFeedVersion(String feedVersion)
    {
        this.feedVersion = feedVersion;
    }

    public Integer getRemoteFeedPageSize()
    {
        return remoteFeedPageSize;
    }

    public void setRemoteFeedPageSize(Integer remoteFeedPageSize)
    {
        this.remoteFeedPageSize = remoteFeedPageSize;
    }

    @Override
    public CustomRepositoryConfiguration getImmutable()
    {
        return new NugetRepositoryConfigurationData(this);
    }
}
