package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import javax.annotation.concurrent.Immutable;

/**
 * @author lepenghui
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(PubLayoutProvider.ALIAS)
public class PubRepositoryConfigurationData
        extends CustomRepositoryConfiguration {

    public PubRepositoryConfigurationData() {

    }

    public PubRepositoryConfigurationData(final PubRepositoryConfigurationDto delegate) {
    }
}
