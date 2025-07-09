package com.folib.yaml.configuration.repository.remote;

import com.folib.providers.NpmLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;


import com.fasterxml.jackson.annotation.JsonTypeName;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(NpmLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class NpmRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData
{

    private Long lastChangeId = 0L;
    private String replicateUrl;

    NpmRemoteRepositoryConfiguration()
    {
    }

    NpmRemoteRepositoryConfiguration(NpmRemoteRepositoryConfigurationDto delegate)
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
