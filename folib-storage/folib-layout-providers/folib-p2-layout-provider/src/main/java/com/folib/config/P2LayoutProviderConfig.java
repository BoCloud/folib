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
package com.folib.config;

import com.folib.providers.layout.P2LayoutProvider;
import com.folib.repository.P2RepositoryFeatures;
import com.folib.repository.P2RepositoryStrategy;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({ "com.folib.repository",
                 "com.folib.providers",
                 "com.folib.services",
                 "com.folib.storage",
               })
public class P2LayoutProviderConfig
{

    @Bean(name = "p2LayoutProvider")
    P2LayoutProvider p2LayoutProvider()
    {
        return new P2LayoutProvider();
    }

    @Bean(name = "p2RepositoryFeatures")
    P2RepositoryFeatures p2RepositoryFeatures()
    {
        return new P2RepositoryFeatures();
    }

    @Bean(name = "p2RepositoryManagementStrategy")
    P2RepositoryStrategy p2RepositoryManagementStrategy()
    {
        return new P2RepositoryStrategy();
    }

}
