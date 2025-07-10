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
package com.folib.providers.repository;

import java.util.Map;
import java.util.stream.Collectors;

import javax.annotation.PostConstruct;
import jakarta.inject.Inject;

import com.folib.providers.AbstractMappedProviderRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

/**
 * @author Veadan
 */
@Component("repositoryProviderRegistry")
public class RepositoryProviderRegistry extends AbstractMappedProviderRegistry<RepositoryProvider>
{

    private static final Logger logger = LoggerFactory.getLogger(RepositoryProviderRegistry.class);


    public RepositoryProviderRegistry()
    {
    }

    @Override
    @PostConstruct
    public void initialize()
    {
        logger.info("Initialized the repository provider registry.");
    }

    public void dump()
    {
        logger.info("Listing repository providers:");
        for (String providerName : getProviders().keySet())
        {
            logger.info(" provider: {}", providerName);
        }
    }

    /**
     * K: String   :
     * V: Provider : The provider
     *
     * @return
     */
    @Override
    public Map<String, RepositoryProvider> getProviders()
    {
        return super.getProviders();
    }

    @Inject
    @Override
    public void setProviders(Map<String, RepositoryProvider> providers)
    {
        super.setProviders(providers.entrySet()
                                    .stream()
                                    .collect(Collectors.toMap(e -> e.getKey()
                                                                    .replace(RepositoryProvider.class.getSimpleName(),
                                                                             ""),
                                                              e -> e.getValue())));
    }

    @Override
    public RepositoryProvider getProvider(String alias)
    {
        return super.getProvider(alias);
    }

    @Override
    public RepositoryProvider addProvider(String alias, RepositoryProvider provider)
    {
        return super.addProvider(alias, provider);
    }

    @Override
    public void removeProvider(String alias)
    {
        super.removeProvider(alias);
    }

}
