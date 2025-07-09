package com.folib.configuration;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.CocoapodsLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import javax.annotation.concurrent.Immutable;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(CocoapodsLayoutProvider.ALIAS)
public class CocoapodsRepositoryConfigurationData
        extends CustomRepositoryConfiguration
{

    private boolean allowsUnpublish;

    public CocoapodsRepositoryConfigurationData()
    {

    }

    public CocoapodsRepositoryConfigurationData(final CocoapodsRepositoryConfigurationDto delegate)
    {
        this.allowsUnpublish = delegate.allowsUnpublish();
    }

    public boolean isAllowsUnpublish()
    {
        return allowsUnpublish;
    }
}
