package com.folib.yaml.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.ConanLayoutProvider;
import com.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

import javax.annotation.concurrent.Immutable;

@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(ConanLayoutProvider.ALIAS)
public class ConanRepositoryConfigurationData extends CustomRepositoryConfiguration {
    private boolean allowsUnpublish;

    public ConanRepositoryConfigurationData() {

    }

    public ConanRepositoryConfigurationData(final ConanRepositoryConfigurationDto delegate) {
        this.allowsUnpublish = delegate.allowsUnpublish();
    }

    public boolean isAllowsUnpublish() {
        return allowsUnpublish;
    }

}