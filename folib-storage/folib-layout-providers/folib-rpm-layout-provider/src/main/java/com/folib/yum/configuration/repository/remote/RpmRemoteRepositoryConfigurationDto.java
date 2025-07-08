package com.folib.yum.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.folib.providers.layout.RpmLayoutProvider;
import com.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;
@JsonTypeName(RpmLayoutProvider.ALIAS)
public class RpmRemoteRepositoryConfigurationDto extends RemoteRepositoryConfigurationDto
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
        return new RpmRemoteRepositoryConfiguration(this);
    }

}
