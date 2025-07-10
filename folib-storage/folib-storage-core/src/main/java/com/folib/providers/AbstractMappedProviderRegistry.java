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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Veadan
 */
public abstract class AbstractMappedProviderRegistry<T>
{
    
    private static final Logger logger = LoggerFactory.getLogger(AbstractMappedProviderRegistry.class);

    /**
     * K: The alias
     * V: The provider implementation
     */
    private Map<String, T> providers = new LinkedHashMap<>();


    public AbstractMappedProviderRegistry()
    {
    }

    public abstract void initialize();

    public Map<String, T> getProviders()
    {
        return providers;
    }

    public void setProviders(Map<String, T> providers)
    {
        providers.entrySet()
                 .stream()
                 .forEach(e -> logger.info("Registered repository provider '{}' with alias '{}'.",
                                           e.getValue().getClass().getCanonicalName(),
                                           e.getKey()));
        this.providers = providers;
    }

    public T getProvider(String alias)
    {
        return providers.get(alias);
    }

    public T addProvider(String alias, T provider)
    {
        return providers.put(alias, provider);
    }

    public void removeProvider(String alias)
    {
        providers.remove(alias);
    }

}
