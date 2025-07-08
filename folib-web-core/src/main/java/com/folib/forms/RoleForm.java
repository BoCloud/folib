package com.folib.forms;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.folib.forms.users.AccessModelForm;
import com.folib.validation.UniqueRoleName;

import javax.validation.constraints.NotEmpty;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleForm
{

    @NotEmpty(message = "A name must be specified.")
    @UniqueRoleName(message = "Role is already registered.")
    private String name;

    private String description;

    private AccessModelForm accessModel;

    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    public String getDescription()
    {
        return description;
    }

    public void setDescription(String description)
    {
        this.description = description;
    }

    public AccessModelForm getAccessModel()
    {
        return accessModel;
    }

    public void setAccessModel(AccessModelForm accessModel)
    {
        this.accessModel = accessModel;
    }
    
}
