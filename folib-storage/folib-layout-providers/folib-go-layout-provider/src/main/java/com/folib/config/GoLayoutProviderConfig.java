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

import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.layout.GoFileSystem;
import com.folib.providers.layout.GoFileSystemProvider;
import com.folib.providers.layout.GoLayoutProvider;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

/**
 * @author veadan
 * @date 1/3/2024 15:31
 */
@Configuration
@ComponentScan({"com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage"})
public class GoLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + GoLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            GoLayoutProvider.ALIAS;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory GoRepositoryFileSystemProviderFactory(StorageProviderRegistry storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = GoFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public GoFileSystemProvider GoFileSystemProvider(FileSystemProvider provider) {
        return new GoFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory GoRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                  StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory = GoRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return GoRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public GoFileSystem GoRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                 Repository repository,
                                                 FileSystem storageFileSystem,
                                                 LayoutFileSystemProvider provider) {
        return new GoFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }
}
