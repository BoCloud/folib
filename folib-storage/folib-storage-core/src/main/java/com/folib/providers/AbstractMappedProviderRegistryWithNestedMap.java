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
package com.folib.providers;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Veadan
 */
public abstract class AbstractMappedProviderRegistryWithNestedMap<T>
{

    /**
     * K: The alias
     * V: The provider implementation map
     */
    protected Map<String, Map<String, T>> providers;


    public AbstractMappedProviderRegistryWithNestedMap()
    {
    }

    public abstract void initialize();

    public Map<String, Map<String, T>> getProviders()
    {
        return providers;
    }

    public void setProviders(Map<String, Map<String, T>> providers)
    {
        this.providers = providers;
    }

    public Map<String, T> getProviderImplementations(String alias)
    {
        return providers.get(alias);
    }

    public T getProviderImplementation(String alias, String implementation)
            throws ProviderImplementationException
    {
        Map<String, T> map = providers.get(alias);
        if (map != null)
        {
            return map.get(implementation);
        }
        else
        {
            throw new ProviderImplementationException("The requested implementation '" + implementation + "'" +
                                                      " was not found for provider '" + alias  + "'.");
        }
    }

    public void addProviderImplementation(String alias, String implementation, T provider)
    {
        Map<String, T> map = providers.containsKey(alias) ? providers.get(alias) : new LinkedHashMap<>();
        map.put(implementation, provider);

        providers.put(alias, map);
    }

    public void removeProviderImplementation(String alias, String implementation)
    {
        providers.get(alias).remove(implementation);
    }

}
