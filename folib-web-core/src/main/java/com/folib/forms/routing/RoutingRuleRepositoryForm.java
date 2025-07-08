package com.folib.forms.routing;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.validation.configuration.routing.RoutingRuleRepositoryFormValid;

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
