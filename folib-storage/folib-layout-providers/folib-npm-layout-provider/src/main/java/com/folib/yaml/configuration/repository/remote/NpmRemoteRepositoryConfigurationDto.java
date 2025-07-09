package com.folib.yaml.configuration.repository.remote;

import com.folib.providers.NpmLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

import com.fasterxml.jackson.annotation.JsonTypeName;

/**
 * @author Veadan
 */
@JsonTypeName(NpmLayoutProvider.ALIAS)
public class NpmRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto
{

    private Long lastChangeId = 0L;

    private String replicateUrl;

    public Long getLastChangeId()
    {
        return lastChangeId;
    }

    public void setLastChangeId(Long lastChangeId)
    {
        this.lastChangeId = lastChangeId;
    }

    public String getReplicateUrl()
    {
        return replicateUrl;
    }

    public void setReplicateUrl(String replicateUrl)
    {
        this.replicateUrl = replicateUrl;
    }

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable()
    {
        return new NpmRemoteRepositoryConfiguration(this);
    }

}
