package com.veadan.folib.yaml.configuration.repository.remote;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.veadan.folib.providers.layout.PubLayoutProvider;
import com.veadan.folib.yaml.repository.remote.CustomRemoteRepositoryConfigurationData;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@JsonTypeName(PubLayoutProvider.ALIAS)
@XmlAccessorType(XmlAccessType.FIELD)
public class PubRemoteRepositoryConfiguration extends CustomRemoteRepositoryConfigurationData {

    private Long lastChangeId = 0L;
    private String replicateUrl;

    PubRemoteRepositoryConfiguration() {
    }

    PubRemoteRepositoryConfiguration(PubRemoteRepositoryConfigurationDto delegate) {
        this.lastChangeId = delegate.getLastChangeId();
        this.replicateUrl = delegate.getReplicateUrl();
    }

    public Long getLastChangeId() {
        return lastChangeId;
    }

    public String getReplicateUrl() {
        return replicateUrl;
    }

}
