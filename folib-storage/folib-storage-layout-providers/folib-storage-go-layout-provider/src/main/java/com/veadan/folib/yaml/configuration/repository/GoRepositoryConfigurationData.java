package com.veadan.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.GoLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

/**
 * @author pengYongQiang
 * @date 1/3/2024 15:31
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(GoLayoutProvider.ALIAS)
public class GoRepositoryConfigurationData
        extends CustomRepositoryConfiguration {

    public GoRepositoryConfigurationData() {

    }

    public GoRepositoryConfigurationData(final GoRepositoryConfigurationDto delegate) {
        // maybe one day I'll have some implementation here :)
    }

}
