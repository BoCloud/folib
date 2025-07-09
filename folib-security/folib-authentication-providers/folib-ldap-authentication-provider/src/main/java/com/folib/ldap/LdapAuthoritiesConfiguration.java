/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
