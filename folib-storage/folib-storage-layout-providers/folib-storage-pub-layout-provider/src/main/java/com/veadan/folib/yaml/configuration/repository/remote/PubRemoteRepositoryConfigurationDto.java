package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;
import com.veadan.folib.yaml.repository.remote.RemoteRepositoryConfigurationDto;

/**
 * @author leipenghui
 */
@JsonTypeName(PubLayoutProvider.ALIAS)
public class PubRemoteRepositoryConfigurationDto
        extends RemoteRepositoryConfigurationDto {

    private Long lastChangeId = 0L;

    private String replicateUrl;

    public Long getLastChangeId() {
        return lastChangeId;
    }

    public void setLastChangeId(Long lastChangeId) {
        this.lastChangeId = lastChangeId;
    }

    public String getReplicateUrl() {
        return replicateUrl;
    }

    public void setReplicateUrl(String replicateUrl) {
        this.replicateUrl = replicateUrl;
    }

    @Override
    public CustomRemoteRepositoryConfigurationData getImmutable() {
        return new PubRemoteRepositoryConfiguration(this);
    }

}
