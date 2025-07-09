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
package com.folib.authentication.support;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.google.common.collect.Sets;
import com.folib.users.domain.SystemRole;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.authority.mapping.GrantedAuthoritiesMapper;
import org.springframework.util.Assert;

import com.google.common.collect.ImmutableMap;

/**
 * @author veadan
 */
public class AuthoritiesExternalToInternalMapper
        implements InitializingBean,
                   GrantedAuthoritiesMapper
{

    private static final Logger logger = LoggerFactory.getLogger(AuthoritiesExternalToInternalMapper.class);

    private Map<String, String> rolesMapping;

    @Override
    public Collection<? extends GrantedAuthority> mapAuthorities(Collection<? extends GrantedAuthority> externalAuthorities)
    {
        logger.info("Map authorities [{}]", externalAuthorities);
        Collection<? extends GrantedAuthority> result = externalAuthorities.stream()
                                                                           .map(GrantedAuthority::getAuthority)
                                                                           .map(a -> getRolesMapping().get(a))
                                                                           .filter(Objects::nonNull)
                                                                           .map(SimpleGrantedAuthority::new)
                                                                           .collect(Collectors.toSet());
        logger.info("Authorities mapped [{}]", result);
        if (CollectionUtils.isEmpty(result)) {
            result = Sets.newHashSet(new SimpleGrantedAuthority(SystemRole.GENERAL.name()));
            logger.info("Authorities mapped is null set role is {}", result);
        }
        return result;
    }

    @Override
    public void afterPropertiesSet()
    {
        Assert.notEmpty(rolesMapping, "rolesMapping property not set");
    }

    /**
     * @return Returns an immutable map containing the same entries as {@link #rolesMapping}
     */
    public Map<String, String> getRolesMapping()
    {
        return rolesMapping;
    }

    public void setRolesMapping(Map<String, String> rolesMapping)
    {
        Assert.notNull(rolesMapping, "rolesMapping cannot be null");
        this.rolesMapping = ImmutableMap.copyOf(rolesMapping);
    }

}
