package com.folib.yaml.configuration.repository;

import com.folib.providers.layout.NpmLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;

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
@JsonTypeName(NpmLayoutProvider.ALIAS)
public class NpmRepositoryConfigurationData
        extends CustomRepositoryConfiguration
{

    private boolean allowsUnpublish;

    public NpmRepositoryConfigurationData()
    {

    }

    public NpmRepositoryConfigurationData(final NpmRepositoryConfigurationDto delegate)
    {
        this.allowsUnpublish = delegate.allowsUnpublish();
    }

    public boolean isAllowsUnpublish()
    {
        return allowsUnpublish;
    }
}
