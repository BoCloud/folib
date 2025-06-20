package com.veadan.folib.dto.storage.routing;

import com.veadan.folib.validation.configuration.routing.RoutingRuleRepositoryDtoValid;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@RoutingRuleRepositoryDtoValid
public class RoutingRuleRepositoryDto
{
    private String repositoryId;

    private String storageId;

    public String getRepositoryId()
    {
        return repositoryId;
    }

    public void setRepositoryId(String repositoryId)
    {
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
}
