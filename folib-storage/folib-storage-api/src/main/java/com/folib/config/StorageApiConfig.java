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

import com.folib.booters.StorageBooter;
import com.folib.storage.checksum.ChecksumCacheManager;
import com.folib.storage.validation.ArtifactCoordinatesValidator;

import jakarta.inject.Inject;
import java.util.LinkedHashSet;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;

@Configuration
@ComponentScan({ "com.folib.artifact",
                 "com.folib.configuration",
                 "com.folib.io",
                 "com.folib.providers",
                 "com.folib.services",
                 "com.folib.storage",
                 "com.folib.yaml",
                 "com.folib.dependency",
                 "com.folib.domain",
                 "com.folib.booters",
                 "com.folib.component"
})
@EnableAsync
public class StorageApiConfig
{

    @Inject
    private List<ArtifactCoordinatesValidator> versionValidators;

    @Bean
    ChecksumCacheManager checksumCacheManager()
    {
        ChecksumCacheManager checksumCacheManager = new ChecksumCacheManager();
        checksumCacheManager.setCachedChecksumExpiredCheckInterval(300000);
        checksumCacheManager.setCachedChecksumLifetime(60000);

        return checksumCacheManager;
    }

    @Bean
    LinkedHashSet<ArtifactCoordinatesValidator> versionValidators()
    {
        return new LinkedHashSet<>(versionValidators);
    }


    @Bean
    StorageBooter storageBooter()
    {
        return new StorageBooter();
    }

}
