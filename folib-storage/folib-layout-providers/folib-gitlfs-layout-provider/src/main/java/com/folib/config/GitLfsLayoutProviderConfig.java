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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.folib.booters.PropertiesBooter;
import com.folib.providers.io.LayoutFileSystemFactory;
import com.folib.providers.io.LayoutFileSystemProviderFactory;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.GitFlsFileSystem;
import com.folib.providers.GitFlsFileSystemProvider;
import com.folib.providers.GitLfsLayoutProvider;
import com.folib.providers.storage.StorageProvider;
import com.folib.providers.storage.StorageProviderRegistry;
import com.folib.storage.repository.Repository;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import javax.inject.Qualifier;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.nio.file.FileSystem;
import java.nio.file.spi.FileSystemProvider;

import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Configuration
@ComponentScan({"com.folib.repository",
        "com.folib.providers",
        "com.folib.services",
        "com.folib.storage"})
public class GitLfsLayoutProviderConfig {

    public static final String FILE_SYSTEM_ALIAS = "LayoutFileSystemFactory." + GitLfsLayoutProvider.ALIAS;

    public static final String FILE_SYSTEM_PROVIDER_ALIAS = "LayoutFileSystemProviderFactory." +
            GitLfsLayoutProvider.ALIAS;

    @Bean
    @GitLfsObjectMapper
    public ObjectMapper gitLfsObjectMapper() {
        return GitLfsJacksonMapperFactory.createObjectMapper();
    }

    @Bean(FILE_SYSTEM_PROVIDER_ALIAS)
    public LayoutFileSystemProviderFactory gitLfsRepositoryFileSystemProviderFactory(StorageProviderRegistry
                                                                                             storageProviderRegistry) {
        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            LayoutFileSystemProvider result = gitLfsFileSystemProvider(storageProvider.getFileSystemProvider());

            return result;
        };

    }

    @Bean
    @Scope("prototype")
    public GitFlsFileSystemProvider gitLfsFileSystemProvider(FileSystemProvider provider) {
        return new GitFlsFileSystemProvider(provider);
    }

    @Bean(FILE_SYSTEM_ALIAS)
    public LayoutFileSystemFactory gitLfsRepositoryFileSystemFactory(PropertiesBooter propertiesBooter,
                                                                     StorageProviderRegistry storageProviderRegistry) {
        LayoutFileSystemProviderFactory providerFactory = gitLfsRepositoryFileSystemProviderFactory(storageProviderRegistry);

        return (repository) -> {
            StorageProvider storageProvider = storageProviderRegistry.getProvider(repository.getStorageProvider());

            return gitlfsRepositoryFileSystem(propertiesBooter, repository, storageProvider.getFileSystem(),
                    providerFactory.create(repository));
        };
    }

    @Bean
    @Scope("prototype")
    public GitFlsFileSystem gitlfsRepositoryFileSystem(PropertiesBooter propertiesBooter,
                                                       Repository repository,
                                                       FileSystem storageFileSystem,
                                                       LayoutFileSystemProvider provider) {
        return new GitFlsFileSystem(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Documented
    @Qualifier
    @Retention(RUNTIME)
    public static @interface GitLfsObjectMapper {

    }

}