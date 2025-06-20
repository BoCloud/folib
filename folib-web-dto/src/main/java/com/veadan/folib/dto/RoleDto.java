package com.veadan.folib.dto;

import javax.validation.constraints.NotEmpty;

import com.veadan.folib.dto.users.AccessModelDto;
import com.veadan.folib.validation.UniqueRoleName;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * @author Veadan
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class RoleDto
{

    @NotEmpty(message = "A name must be specified.")
    @UniqueRoleName(message = "Role is already registered.")
    private String name;

    private String description;

    private AccessModelDto accessModel;

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

    public AccessModelDto getAccessModel()
    {
        return accessModel;
    }

    public void setAccessModel(AccessModelDto accessModel)
    {
        this.accessModel = accessModel;
    }
    
}
