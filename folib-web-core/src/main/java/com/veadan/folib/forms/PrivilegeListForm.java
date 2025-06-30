package com.veadan.folib.forms;

import com.veadan.folib.users.domain.Privileges;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * @author Veadan
 */
public class PrivilegeListForm
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
