package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.DebianLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
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
@JsonTypeName(DebianLayoutProvider.ALIAS)
public class DebianRepositoryConfigurationData
        extends CustomRepositoryConfiguration {

    public DebianRepositoryConfigurationData() {

    }

    public DebianRepositoryConfigurationData(final DebianRepositoryConfigurationDto delegate) {
    }
}
