package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.NpmLayoutProvider;
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
