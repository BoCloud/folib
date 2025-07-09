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

import java.util.Collection;

import org.springframework.ldap.core.DirContextOperations;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.mapping.GrantedAuthoritiesMapper;
import org.springframework.security.ldap.authentication.NullLdapAuthoritiesPopulator;
import org.springframework.security.ldap.userdetails.LdapAuthoritiesPopulator;

/**
 * @author veadan
 *
 */
public class MappedAuthoritiesPopulator implements LdapAuthoritiesPopulator
{

    private final GrantedAuthoritiesMapper authoritiesMapper;

    private LdapAuthoritiesPopulator delegate = new NullLdapAuthoritiesPopulator();

    public MappedAuthoritiesPopulator(GrantedAuthoritiesMapper authoritiesMapper)
    {
        super();
        this.authoritiesMapper = authoritiesMapper;
    }

    public LdapAuthoritiesPopulator getDelegate()
    {
        return delegate;
    }

    public void setDelegate(LdapAuthoritiesPopulator delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Collection<? extends GrantedAuthority> getGrantedAuthorities(DirContextOperations userData,
                                                                        String username)
    {
        return authoritiesMapper.mapAuthorities(getDelegate().getGrantedAuthorities(userData, username));
    }

}
