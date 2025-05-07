package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.NugetLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;

import javax.annotation.concurrent.Immutable;

import com.fasterxml.jackson.annotation.JsonTypeName;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(NugetLayoutProvider.ALIAS)
public class NugetRepositoryConfigurationData
        extends CustomRepositoryConfiguration implements NugetRepositoryConfiguration
{

    private String feedVersion;

    private Integer remoteFeedPageSize;

    public NugetRepositoryConfigurationData()
    {

    }


    public NugetRepositoryConfigurationData(final NugetRepositoryConfigurationDto delegate)
    {
        this.feedVersion = delegate.getFeedVersion();
        this.remoteFeedPageSize = delegate.getRemoteFeedPageSize();
    }

    public String getFeedVersion()
    {
        return feedVersion;
    }

    public Integer getRemoteFeedPageSize()
    {
        return remoteFeedPageSize;
    }
}
