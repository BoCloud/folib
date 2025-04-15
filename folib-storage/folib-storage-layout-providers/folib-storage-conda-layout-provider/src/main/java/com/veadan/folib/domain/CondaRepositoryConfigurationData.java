package com.veadan.folib.domain;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import com.veadan.folib.providers.layout.CondaLayoutProvider;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(CondaLayoutProvider.ALIAS)
public class CondaRepositoryConfigurationData extends CustomRepositoryConfiguration {

    private boolean allowsUnpublish;

    public CondaRepositoryConfigurationData() {

    }

    public CondaRepositoryConfigurationData(final CondaRepositoryConfigurationDto delegate) {
        this.allowsUnpublish = delegate.allowsUnpublish();
    }

    public boolean isAllowsUnpublish() {
        return allowsUnpublish;
    }
}
