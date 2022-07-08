package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.fasterxml.jackson.annotation.JsonTypeName;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(Maven2LayoutProvider.ALIAS)
public class MavenRepositoryConfigurationData
        extends CustomRepositoryConfiguration implements MavenRepositoryConfiguration
{

    private boolean indexingEnabled;

    private boolean indexingClassNamesEnabled;

    private String cronExpression;

    private String metadataExpirationStrategy;

    public MavenRepositoryConfigurationData()
    {
    }

    public MavenRepositoryConfigurationData(final MavenRepositoryConfigurationDto delegate)
    {
        this.indexingEnabled = delegate.isIndexingEnabled();
        this.indexingClassNamesEnabled = delegate.isIndexingClassNamesEnabled();
        this.cronExpression = delegate.getCronExpression();
        this.metadataExpirationStrategy = delegate.getMetadataExpirationStrategy();
    }

    public boolean isIndexingEnabled()
    {
        return indexingEnabled;
    }

    public boolean isIndexingClassNamesEnabled()
    {
        return indexingClassNamesEnabled;
    }

    @Override
    public String getCronExpression()
    {
        return cronExpression;
    }

    @Override
    public String getMetadataExpirationStrategy()
    {
        return metadataExpirationStrategy;
    }

}

