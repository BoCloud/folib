package com.veadan.folib.dto;

import java.util.List;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.veadan.folib.users.domain.Privileges;

/**
 * @author Veadan
 */
public class PrivilegeListDto
{

    @NotEmpty
    private List<@NotNull Privileges> privileges;

    public List<Privileges> getPrivileges()
    {
        return privileges;
    }

    public void setPrivileges(List<Privileges> privileges)
    {
        this.privileges = privileges;
    }

}
