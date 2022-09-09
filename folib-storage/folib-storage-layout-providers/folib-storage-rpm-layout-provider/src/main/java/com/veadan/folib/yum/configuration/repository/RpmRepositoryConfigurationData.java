package com.veadan.folib.yum.configuration.repository;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.RpmLayoutProvider;
import com.veadan.folib.yaml.repository.CustomRepositoryConfiguration;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
@JsonTypeName(RpmLayoutProvider.ALIAS)
public class RpmRepositoryConfigurationData  extends CustomRepositoryConfiguration {
    private boolean allowsUnpublish;

    public RpmRepositoryConfigurationData()
    {

    }

    public RpmRepositoryConfigurationData(final RpmRepositoryConfigurationDto delegate)
    {
        this.allowsUnpublish = delegate.allowsUnpublish();
    }

    public boolean isAllowsUnpublish()
    {
        return allowsUnpublish;
    }
}

