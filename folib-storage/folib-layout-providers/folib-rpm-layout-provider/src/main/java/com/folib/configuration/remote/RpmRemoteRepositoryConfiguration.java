package com.folib.configuration.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.RpmLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;


@JsonTypeName(RpmLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class RpmRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData
{

    private Long lastChangeId = 0L;
    private String replicateUrl;

    RpmRemoteRepositoryConfiguration()
    {
    }

    RpmRemoteRepositoryConfiguration(RpmRemoteRepositoryConfigurationDto delegate)
    {
        this.lastChangeId = delegate.getLastChangeId();
        this.replicateUrl = delegate.getReplicateUrl();
    }

    public Long getLastChangeId()
    {
        return lastChangeId;
    }

    public String getReplicateUrl()
    {
        return replicateUrl;
    }

}

