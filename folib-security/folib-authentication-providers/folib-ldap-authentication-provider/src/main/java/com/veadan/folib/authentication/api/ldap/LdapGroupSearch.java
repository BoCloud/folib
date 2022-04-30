package com.veadan.folib.authentication.api.ldap;

import javax.validation.constraints.NotEmpty;

public class LdapGroupSearch
{

    @NotEmpty
    private String groupSearchBase;

    @NotEmpty
    private String groupSearchFilter;

    public String getGroupSearchBase()
    {
        return groupSearchBase;
    }

    public void setGroupSearchBase(String groupSearchBase)
    {
        this.groupSearchBase = groupSearchBase;
    }

    public String getGroupSearchFilter()
    {
        return groupSearchFilter;
    }

    public void setGroupSearchFilter(String groupSearchFilter)
    {
        this.groupSearchFilter = groupSearchFilter;
    }

}
