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
package com.folib.providers.io;

import com.folib.storage.repository.Repository;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RepositoryFileSystemRegistry
{

    private Map<String, LayoutFileSystemProviderFactory> fileSystemProviderFactoryMap = new HashMap<>();

    private Map<String, LayoutFileSystemFactory> fileSystemFactoryMap = new HashMap<>();

    @Autowired(required = false)
    public void setFileSystemProviderFactories(Map<String, LayoutFileSystemProviderFactory> factories)
    {
        factories.entrySet()
                 .stream()
                 .forEach(e -> fileSystemProviderFactoryMap.put(extractLayoutAlias(LayoutFileSystemProviderFactory.class,
                                                                                   e.getKey()),
                                                                e.getValue()));
    }

    @Autowired(required = false)
    public void setFileSystemFactories(Map<String, LayoutFileSystemFactory> factories)
    {
        factories.entrySet()
                 .stream()
                 .forEach(e -> fileSystemFactoryMap.put(extractLayoutAlias(LayoutFileSystemFactory.class,
                                                                           e.getKey()),
                                                        e.getValue()));
    }

    private String extractLayoutAlias(Class<?> factoryClass,
                                      String alias)
    {
        return alias.replace(factoryClass.getSimpleName(), "").substring(1);
    }

    public LayoutFileSystemFactory lookupRepositoryFileSystemFactory(Repository r)
    {
        return fileSystemFactoryMap.get(r.getLayout());
    }

    public LayoutFileSystemProviderFactory lookupRepositoryFileSystemProviderFactory(Repository r)
    {
        return fileSystemProviderFactoryMap.get(r.getLayout());
    }
}
