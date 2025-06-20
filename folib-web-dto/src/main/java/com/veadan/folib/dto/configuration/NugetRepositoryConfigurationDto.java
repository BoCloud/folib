package com.veadan.folib.dto.configuration;

import com.veadan.folib.providers.layout.NugetLayoutProvider;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author veadan
 */
@JsonTypeName(NugetLayoutProvider.ALIAS)
public class NugetRepositoryConfigurationDto
        extends CustomRepositoryConfigurationDto
{

    private String feedVersion;

    private Integer remoteFeedPageSize;

    public String getFeedVersion()
    {
        return feedVersion;
    }

    public void setFeedVersion(final String feedVersion)
    {
        this.feedVersion = feedVersion;
    }

    public Integer getRemoteFeedPageSize()
    {
        return remoteFeedPageSize;
    }

    public void setRemoteFeedPageSize(final Integer remoteFeedPageSize)
    {
        this.remoteFeedPageSize = remoteFeedPageSize;
    }

    @Override
    public <T> T accept(final CustomRepositoryConfigurationDtoVisitor<T> visitor)
    {
        return visitor.visit(this);
    }

}
