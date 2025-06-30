package com.veadan.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.Maven2LayoutProvider;
import com.veadan.folib.storage.metadata.maven.MetadataExpirationStrategyType;
import com.veadan.folib.validation.configuration.DescribableEnumValue;

/**
 * @author veadan
 */
@JsonTypeName(Maven2LayoutProvider.ALIAS)
public class MavenRepositoryConfigurationForm
        extends CustomRepositoryConfigurationForm
{

    private boolean indexingEnabled;

    private boolean indexingClassNamesEnabled;

    private String cronExpression;

    @DescribableEnumValue(type = MetadataExpirationStrategyType.class,
                          message = "The metadataExpirationStrategy can be either \"checksum\", or \"refresh\"")
    private String metadataExpirationStrategy;

    public boolean isIndexingEnabled()
    {
        return indexingEnabled;
    }

    public void setIndexingEnabled(final boolean indexingEnabled)
    {
        this.indexingEnabled = indexingEnabled;
    }

    public boolean isIndexingClassNamesEnabled()
    {
        return indexingClassNamesEnabled;
    }

    public void setIndexingClassNamesEnabled(final boolean indexingClassNamesEnabled)
    {
        this.indexingClassNamesEnabled = indexingClassNamesEnabled;
    }

    public String getCronExpression() { return cronExpression; }

    public void setCronExpression(String cronExpression)
    {
        this.cronExpression = cronExpression;
    }

    public String getMetadataExpirationStrategy()
    {
        return metadataExpirationStrategy;
    }

    public void setMetadataExpirationStrategy(String metadataExpirationStrategy)
    {
        this.metadataExpirationStrategy = metadataExpirationStrategy;
    }

    @Override
    public <T> T accept(final CustomRepositoryConfigurationFormVisitor<T> visitor)
    {
        return visitor.visit(this);
    }

}
