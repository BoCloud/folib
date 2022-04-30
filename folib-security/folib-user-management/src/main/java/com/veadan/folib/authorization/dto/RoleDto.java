package com.veadan.folib.authorization.dto;

import java.io.Serializable;

import com.veadan.folib.users.domain.Privileges;
import com.veadan.folib.users.dto.AccessModelDto;

/**
 * @author mtodorov
 */
public class RoleDto
        implements Serializable, Role
{

    private String name;

    private String description;

    private AccessModelDto accessModel;


    public RoleDto()
    {
    }

    public RoleDto(String name,
                   String description,
                   AccessModelDto accessModel)
    {
        this.name = name;
        this.description = description;
        this.accessModel = accessModel;
    }

    @Override
    public String getName()
    {
        return name;
    }

    public void setName(String name)
    {
        this.name = name;
    }

    @Override
    public String getDescription()
    {
        return description;
    }

    public void setDescription(String description)
    {
        this.description = description;
    }

    @Override
    public AccessModelDto getAccessModel()
    {
        return accessModel;
    }

    public void setAccessModel(AccessModelDto accessModel)
    {
        this.accessModel = accessModel;
    }

    public void addPrivilege(Privileges p)
    {
        accessModel.getApiAuthorities().add(p);
    }
    
    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder("\n\t\tRole{");
        sb.append("name='").append(name).append('\'');
        sb.append(", description='").append(description);
        sb.append('}');

        return sb.toString();
    }

}
