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

import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.NotEmpty;

import com.folib.authentication.support.ExternalRoleMapping;
import com.folib.validation.LdapUri;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonUnwrapped;

/**
 * @author veadan
 */
public class LdapConfiguration
{

    @LdapUri
    @NotEmpty
    private String url;

    private String managerDn;

    private String managerPassword;

    private boolean userPasswordEncoded;

    private LdapAuthoritiesConfiguration authoritiesConfiguration = new LdapAuthoritiesConfiguration();

    private LdapUserSearch userSearch = new LdapUserSearch();

    private List<ExternalRoleMapping> roleMappingList = new ArrayList<>();

    private List<String> userDnPatternList = new ArrayList<>();

    @JsonInclude(JsonInclude.Include.NON_EMPTY)
    private boolean enableProvider;

    public String getUrl()
    {
        return url;
    }

    public void setUrl(String url)
    {
        this.url = url;
    }

    public String getManagerDn()
    {
        return managerDn;
    }

    public void setManagerDn(String managerDn)
    {
        this.managerDn = managerDn;
    }

    public String getManagerPassword()
    {
        return managerPassword;
    }

    @JsonProperty("authorities")
    public LdapAuthoritiesConfiguration getAuthoritiesConfiguration()
    {
        return authoritiesConfiguration;
    }

    public void setAuthoritiesConfiguration(LdapAuthoritiesConfiguration authoritiesConfiguration)
    {
        this.authoritiesConfiguration = authoritiesConfiguration;
    }

    @JsonUnwrapped
    public LdapUserSearch getUserSearch()
    {
        return userSearch;
    }

    public void setUserSearch(LdapUserSearch userSearch)
    {
        this.userSearch = userSearch;
    }

    public void setManagerPassword(String managerPassword)
    {
        this.managerPassword = managerPassword;
    }

    public List<ExternalRoleMapping> getRoleMappingList()
    {
        return roleMappingList;
    }

    public void setRoleMappingList(List<ExternalRoleMapping> roleMappingList)
    {
        this.roleMappingList = roleMappingList;
    }

    public List<String> getUserDnPatternList()
    {
        return userDnPatternList;
    }

    public void setUserDnPatternList(List<String> userDnPatternList)
    {
        this.userDnPatternList = userDnPatternList;
    }

    public boolean getEnableProvider()
    {
        return enableProvider;
    }

    public void setEnableProvider(boolean enableProvider)
    {
        this.enableProvider = enableProvider;
    }

    public boolean isUserPasswordEncoded()
    {
        return userPasswordEncoded;
    }

    public void setUserPasswordEncoded(boolean userPasswordEncoded)
    {
        this.userPasswordEncoded = userPasswordEncoded;
    }
}
