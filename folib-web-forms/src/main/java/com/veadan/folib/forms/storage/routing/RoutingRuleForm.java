package com.veadan.folib.forms.storage.routing;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.veadan.folib.storage.routing.RoutingRuleTypeEnum;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Pablo Tirado
 * @author Przemyslaw Fusik
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoutingRuleForm
{

    private String storageId;

    private String groupRepositoryId;

    @NotBlank(message = "A pattern must be specified.")
    private String pattern;

    @NotNull(message = "A type must be specified.")
    private RoutingRuleTypeEnum type;

    @Valid
    private List<RoutingRuleRepositoryForm> repositories = new ArrayList<>();

    public String getStorageId()
    {
        return storageId;
    }

    public void setStorageId(String storageId)
    {
        this.storageId = storageId;
    }

    public String getGroupRepositoryId()
    {
        return groupRepositoryId;
    }

    public void setGroupRepositoryId(String groupRepositoryId)
    {
        this.groupRepositoryId = groupRepositoryId;
    }

    public String getPattern()
    {
        return pattern;
    }

    public void setPattern(String pattern)
    {
        this.pattern = pattern;
    }

    public RoutingRuleTypeEnum getType()
    {
        return type;
    }

    public void setType(RoutingRuleTypeEnum type)
    {
        this.type = type;
    }

    public List<RoutingRuleRepositoryForm> getRepositories()
    {
        return repositories;
    }

    public void setRepositories(List<RoutingRuleRepositoryForm> repositories)
    {
        this.repositories = repositories;
    }
}
