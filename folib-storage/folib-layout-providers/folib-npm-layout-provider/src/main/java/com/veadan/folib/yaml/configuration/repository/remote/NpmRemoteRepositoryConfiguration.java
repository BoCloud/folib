package com.veadan.folib.yaml.configuration.repository.remote;

import com.veadan.folib.providers.layout.NpmLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;


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
