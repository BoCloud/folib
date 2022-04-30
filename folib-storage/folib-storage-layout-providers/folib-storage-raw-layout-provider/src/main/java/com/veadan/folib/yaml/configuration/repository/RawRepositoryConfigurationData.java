package com.veadan.folib.yaml.configuration.repository;

import com.veadan.folib.providers.layout.RawLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.fasterxml.jackson.annotation.JsonTypeName;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

/**
 * @author Przemyslaw Fusik
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
