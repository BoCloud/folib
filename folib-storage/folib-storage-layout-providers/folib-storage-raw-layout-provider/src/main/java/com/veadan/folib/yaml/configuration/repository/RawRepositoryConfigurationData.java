package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.RawLayoutProvider;
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
@JsonTypeName(RawLayoutProvider.ALIAS)
public class RawRepositoryConfigurationData
        extends CustomRepositoryConfiguration
{

    public RawRepositoryConfigurationData()
    {

    }

    public RawRepositoryConfigurationData(final RawRepositoryConfigurationDto delegate)
    {
        // maybe one day I'll have some implementation here :)
    }

}
