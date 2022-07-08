package com.veadan.folib.storage.routing;

import java.io.Serializable;

import com.veadan.folib.configuration.ConfigurationUtils;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author veadan
 * @author Pablo Tirado
 */
public class MutableRoutingRuleRepository
        implements Serializable
{

    private String storageId;

    private String repositoryId;

    public MutableRoutingRuleRepository()
    {
    }

    public MutableRoutingRuleRepository(String storageAndRepositoryId)
    {
        this(ConfigurationUtils.getStorageId(null, storageAndRepositoryId),
             ConfigurationUtils.getRepositoryId(storageAndRepositoryId));
    }

    
    @JsonCreator
    public MutableRoutingRuleRepository(@JsonProperty("storageId") String storageId,
                                        @JsonProperty("repositoryId") String repositoryId)
    {
        this.storageId = storageId;
        this.repositoryId = repositoryId;
    }

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId)
    {
        this.repositoryId = repositoryId;
    }
    
}
