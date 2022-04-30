package com.veadan.folib.authentication.api.ldap;

import javax.validation.constraints.NotEmpty;

/**
 * @author ankit.tomar
 */
public class LdapUserSearch
{

    @NotEmpty
    private String userSearchBase;

    @NotEmpty
    private String userSearchFilter;

    public String getUserSearchBase()
    {
        return userSearchBase;
    }

    public void setUserSearchBase(String userSearchBase)
    {
        this.userSearchBase = userSearchBase;
    }

    public String getUserSearchFilter()
    {
        return userSearchFilter;
    }

    public void setUserSearchFilter(String userSearchFilter)
    {
        this.userSearchFilter = userSearchFilter;
    }

}
