package com.veadan.folib.dto;

import javax.validation.Valid;
import java.util.List;

/**
 * @author Veadan
 */
public class RoleListDto
{

    @Valid
    private List<RoleDto> roles;

    public List<RoleDto> getRoles()
    {
        return roles;
    }

    public void setRoles(List<RoleDto> roles)
    {
        this.roles = roles;
    }
}
