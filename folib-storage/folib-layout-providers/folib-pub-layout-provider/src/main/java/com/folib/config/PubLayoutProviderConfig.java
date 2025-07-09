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
import com.folib.providers.PubFileSystem;
import com.folib.providers.PubFileSystemProvider;
import com.folib.providers.PubLayoutProvider;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

@Configuration
@ComponentScan({"com.folib.dependency.snippet",
        "com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage",
        "com.folib.indexer",
})
public class PubLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + PubLayoutProvider.ALIAS;
    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory."
            + PubLayoutProvider.ALIAS;

    @Inject
    protected StorageProviderRegistry storageProviderRegistry;

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory pubRepositoryFileSystemProviderFactory() {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = pubFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public PubFileSystemProvider pubFileSystemProvider(FileSystemProvider provider) {
        return new PubFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory pubRepositoryFileSystemFactory(PropertiesBooter propertiesBooter) {
        LayoutFileSystemProviderFactory providerFactory = pubRepositoryFileSystemProviderFactory();

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return pubRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public PubFileSystem pubRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                 Repository repository,
                                                 FileSystem storageFileSystem,
                                                 LayoutFileSystemProvider provider) {
        return new PubFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

}
