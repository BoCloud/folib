package com.veadan.folib.forms.storage.routing;

import com.veadan.folib.validation.configuration.routing.RoutingRuleRepositoryFormValid;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@RoutingRuleRepositoryFormValid
public class RoutingRuleRepositoryForm
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
