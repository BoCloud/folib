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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import javax.inject.Inject;

import org.springframework.beans.factory.config.AbstractFactoryBean;
import org.springframework.core.env.AbstractEnvironment;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertySource;

public class ExternalRolesMappingFactoryBean extends AbstractFactoryBean<Map<String, String>>
{

    private Environment env;

    private final String externalAuthenticationId;

    public ExternalRolesMappingFactoryBean(String externalAuthenticationId)
    {
        super();
        this.externalAuthenticationId = externalAuthenticationId;
    }

    public Environment getEnv()
    {
        return env;
    }

    @Inject
    public void setEnv(Environment env)
    {
        this.env = env;
    }

    @Override
    public Class<?> getObjectType()
    {
        return Map.class;
    }

    @Override
    protected Map<String, String> createInstance()
        throws Exception
    {
        Map<String, String> roleMappingMap = new HashMap<>();

        PropertySource<?> propertySource = ((AbstractEnvironment) env).getPropertySources()
                                                                      .get("folib-authentication-providers");
        String[] propertyNames = ((EnumerablePropertySource<?>) propertySource).getPropertyNames();

        Map<String, String> externalRolesMap = new HashMap<>();
        Map<String, String> folibRolesMap = new HashMap<>();

        for (String propertyName : propertyNames)
        {
            String prefix = String.format("folib.authentication.%s.rolesMapping", externalAuthenticationId);
            
            if (!propertyName.startsWith(prefix))
            {
                continue;
            }
            if (propertyName.endsWith("externalRole"))
            {
                externalRolesMap.put(propertyName.replace(prefix, ""),
                                     env.getProperty(propertyName));
            }
            else if (propertyName.endsWith("folibRole"))
            {
                folibRolesMap.put(propertyName.replace(prefix, ""),
                                      env.getProperty(propertyName));
            }
        }

        for (Entry<String, String> externalRoleEntry : externalRolesMap.entrySet())
        {
            String externalRole = externalRoleEntry.getValue();
            String folibRole = folibRolesMap.get(externalRoleEntry.getKey().replace("externalRole",
                                                                                            "folibRole"));

            roleMappingMap.put(externalRole, folibRole);
        }

        return roleMappingMap;
    }

}
