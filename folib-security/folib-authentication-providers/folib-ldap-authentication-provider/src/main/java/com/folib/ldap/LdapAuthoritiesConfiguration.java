package com.folib.ldap;

import com.fasterxml.jackson.annotation.JsonUnwrapped;

public class LdapAuthoritiesConfiguration
{

    private LdapGroupSearch groupSearch = new LdapGroupSearch();

    private boolean searchSubtree;

    private String groupRoleAttribute;

    private String rolePrefix;

    private boolean convertToUpperCase;

    @JsonUnwrapped
    public LdapGroupSearch getGroupSearch()
    {
        return groupSearch;
    }

    public void setGroupSearch(LdapGroupSearch groupSearch)
    {
        this.groupSearch = groupSearch;
    }

    public boolean isSearchSubtree()
    {
        return searchSubtree;
    }

    public void setSearchSubtree(boolean searchSubtree)
    {
        this.searchSubtree = searchSubtree;
    }

    public String getGroupRoleAttribute()
    {
        return groupRoleAttribute;
    }

    public void setGroupRoleAttribute(String groupRoleAttribute)
    {
        this.groupRoleAttribute = groupRoleAttribute;
    }

    public String getRolePrefix()
    {
        return rolePrefix;
    }

    public void setRolePrefix(String rolePrefix)
    {
        this.rolePrefix = rolePrefix;
    }

    public boolean isConvertToUpperCase()
    {
        return convertToUpperCase;
    }

    public void setConvertToUpperCase(boolean convertToUpperCase)
    {
        this.convertToUpperCase = convertToUpperCase;
    }

}
