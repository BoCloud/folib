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

import com.folib.authentication.api.AuthenticationItem;
import com.folib.authentication.api.AuthenticationItemConfigurationManager;
import com.folib.authentication.api.AuthenticationItems;
import com.folib.authentication.api.CustomAuthenticationItemMapper;
import com.folib.authentication.support.ExternalRoleMapping;

import javax.inject.Inject;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;

@Component
public class LdapAuthenticationConfigurationManager
        implements CustomAuthenticationItemMapper<LdapConfiguration>
{

    private static final String MANAGER_PASSWORD = "managerPassword";

    private static final String MANAGER_DN = "managerDn";

    public static final String AUTHENTICATION_ITEM_LDAP = "ldap";

    public static final String USER_DN_PATTERNS = "userDnPatterns";
    public static final String ROLES_MAPPING = "rolesMapping";
    public static final String CONVERT_TO_UPPER_CASE = "convertToUpperCase";
    public static final String USER_PASSWORD_ENCODED = "userPasswordEncoded";
    public static final String ROLE_PREFIX = "rolePrefix";
    public static final String GROUP_ROLE_ATTRIBUTE = "groupRoleAttribute";
    public static final String GROUP_SEARCH_FILTER = "groupSearchFilter";
    public static final String USER_SEARCH_FILTER = "userSearchFilter";
    public static final String SEARCH_SUBTREE = "searchSubtree";
    public static final String GROUP_SEARCH_BASE = "groupSearchBase";
    public static final String USER_SEARCH_BASE = "userSearchBase";
    public static final String URL = "url";
    public static final String AUTHORITIES = "authorities";

    @Inject
    private AuthenticationItemConfigurationManager authenticationItemConfigurationManager;

    @Inject
    private PasswordEncoder passwordEncoder;

    @Override
    public String getConfigurationItemId()
    {
        return AUTHENTICATION_ITEM_LDAP;
    }

    public LdapConfiguration getConfiguration()
    {
        LdapConfiguration ldapConfiguration = authenticationItemConfigurationManager.getCustomAuthenticationItem(this);

        // TODO: This is a temporary solution to improve the user experience when enabling LDAP as a UserDetailsService.
        AuthenticationItems authenticationItems = authenticationItemConfigurationManager.getAuthenticationItems();
        List<AuthenticationItem> list = authenticationItems.getAuthenticationItemList();
        for (int i=0; i < list.size(); i++)
        {
            AuthenticationItem item = list.get(i);
            if(item.getName().equalsIgnoreCase("ldapUserDetailsService")) {
                ldapConfiguration.setEnableProvider(item.getEnabled());
            }
        }

        return ldapConfiguration;
    }

    public void updateConfiguration(LdapConfiguration configuration)
        throws IOException
    {
        authenticationItemConfigurationManager.putCustomAuthenticationItem(configuration, this);

        // TODO: This is a temporary solution to improve the user experience when enabling LDAP as a UserDetailsService.
        //       We should improve how this works with a later PR.
        AuthenticationItems authenticationItems = authenticationItemConfigurationManager.getAuthenticationItems();
        List<AuthenticationItem> list = authenticationItems.getAuthenticationItemList();
        for (int i = 0; i < list.size(); i++)
        {
            AuthenticationItem item = list.get(i);
            if (item.getName().equalsIgnoreCase("ldapUserDetailsService"))
            {
//                item.setOrder(configuration.getEnableProvider() ? 1 : 0);
                item.setEnabled(configuration.getEnableProvider());
            }
            else if(item.getName().equalsIgnoreCase("yamlUserDetailService"))
            {
//                item.setOrder(configuration.getEnableProvider() ? 0 : 1);
            }
        }

        authenticationItemConfigurationManager.updateAuthenticationItems(authenticationItems);
    }

    public Map<String, Object> mapAuthorities(LdapAuthoritiesConfiguration source)
    {
        Map<String, Object> result = new HashMap<>();

        result.put(GROUP_SEARCH_BASE, source.getGroupSearch().getGroupSearchBase());
        result.put(GROUP_SEARCH_FILTER, source.getGroupSearch().getGroupSearchFilter());

        result.put(SEARCH_SUBTREE, source.isSearchSubtree());
        result.put(GROUP_ROLE_ATTRIBUTE, source.getGroupRoleAttribute());
        result.put(ROLE_PREFIX, source.getRolePrefix());
        result.put(CONVERT_TO_UPPER_CASE, source.isConvertToUpperCase());

        return result;
    }

    @Override
    public Map<String, Object> map(LdapConfiguration source)
    {
        Map<String, Object> result = new HashMap<>();

        result.put(URL, source.getUrl());
        result.put(MANAGER_DN, source.getManagerDn());
        result.put(MANAGER_PASSWORD, source.getManagerPassword());

        result.put(AUTHORITIES, mapAuthorities(source.getAuthoritiesConfiguration()));

        result.put(USER_SEARCH_BASE, source.getUserSearch().getUserSearchBase());
        result.put(USER_SEARCH_FILTER, source.getUserSearch().getUserSearchFilter());

        result.put(ROLES_MAPPING,
                   source.getRoleMappingList()
                         .stream()
                         .map(rm -> new HashMap<String, String>()
                         {
                             {
                                 put("externalRole", rm.getExternalRole());
                                 put("folibRole", rm.getFolibRole());
                             }
                         })
                         .collect(Collectors.toList()));

        result.put(USER_DN_PATTERNS, source.getUserDnPatternList());
        result.put(USER_PASSWORD_ENCODED, source.isUserPasswordEncoded());

        return result;
    }

    public LdapAuthoritiesConfiguration mapAuthorities(Map<String, Object> source)
    {
        LdapAuthoritiesConfiguration result = new LdapAuthoritiesConfiguration();

        result.setSearchSubtree(Boolean.TRUE.equals(source.get(SEARCH_SUBTREE)));
        result.setGroupRoleAttribute((String) source.get(GROUP_ROLE_ATTRIBUTE));
        result.setRolePrefix((String) source.get(ROLE_PREFIX));
        result.setConvertToUpperCase(Boolean.TRUE.equals(source.get(CONVERT_TO_UPPER_CASE)));

        LdapGroupSearch groupSearch = new LdapGroupSearch();
        groupSearch.setGroupSearchBase((String) source.get(GROUP_SEARCH_BASE));
        groupSearch.setGroupSearchFilter((String) source.get(GROUP_SEARCH_FILTER));
        result.setGroupSearch(groupSearch);

        return result;
    }

    @Override
    public LdapConfiguration map(Map<String, Object> source)
    {
        LdapConfiguration result = new LdapConfiguration();

        result.setUrl((String) source.get(URL));
        result.setManagerDn((String) source.get(MANAGER_DN));
        result.setManagerPassword(String.valueOf(source.get(MANAGER_PASSWORD)));
        result.setUserPasswordEncoded(Boolean.TRUE.equals(source.get(USER_PASSWORD_ENCODED)));

        LdapUserSearch userSearch = new LdapUserSearch();
        userSearch.setUserSearchBase((String) source.get(USER_SEARCH_BASE));
        userSearch.setUserSearchFilter((String) source.get(USER_SEARCH_FILTER));
        result.setUserSearch(userSearch);

        result.setAuthoritiesConfiguration(mapAuthorities((Map<String, Object>) source.get(AUTHORITIES)));

        result.setUserDnPatternList((List<String>) source.get(USER_DN_PATTERNS));

        result.setRoleMappingList(((List<Map<String, String>>) source.get(ROLES_MAPPING)).stream()
                                                                                         .map(rm -> new ExternalRoleMapping(
                                                                                                 rm.get("externalRole"),
                                                                                                 rm.get("folibRole")))
                                                                                         .collect(Collectors.toList()));

        return result;
    }

}
