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

import cn.hutool.extra.spring.SpringUtil;
import com.folib.booters.PropertiesBooter;
import com.folib.constant.GlobalConstants;
import com.folib.enums.StorageProviderEnum;
import com.folib.io.StorageFileSystem;
import com.folib.providers.layout.LayoutFileSystemProvider;
import com.folib.providers.layout.LayoutProvider;
import com.folib.services.ConfigurationManagementService;
import com.folib.storage.repository.Repository;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.FileSystem;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Set;

/**
 * This class decorates {@link StorageFileSystem} with common layout specific
 * logic. <br>
 * Root folder is the {@link Repository} base directory.
 *
 * @author @author veadan
 * 
 * @see Repository
 * @see LayoutProvider
 */
public abstract class LayoutFileSystem
        extends StorageFileSystem
{
    public static final String TEMP = ".temp";

    private final Repository repository;
    private final LayoutFileSystemProvider provider;
    private final RootRepositoryPath rootRepositoryPath;
    
    public LayoutFileSystem(PropertiesBooter propertiesBooter,
                            Repository repository,
                            FileSystem storageFileSystem,
                            LayoutFileSystemProvider provider)
    {
        super(repository.getStorage(), propertiesBooter, storageFileSystem);
        this.repository = repository;
        this.provider = provider;
        this.rootRepositoryPath = new RootRepositoryPath(resolveRootPath(), this);
    }

    public Repository getRepository()
    {
        return repository;
    }

    @Override
    public LayoutFileSystemProvider provider()
    {
        return provider;
    }

    public void createRootDirectory() throws IOException
    {
        Path rootPath = resolveRootPath();
        Files.createDirectories(rootPath);
    }
    
    public void cleanupRootDirectory()
        throws IOException
    {
        Path storageRootPath = super.getRootDirectory();
        if (!Files.exists(storageRootPath) || !Files.isDirectory(storageRootPath))
        {
            return;
        }
        
        try (DirectoryStream<Path> dirStream = Files.newDirectoryStream(storageRootPath))
        {
            if (dirStream.iterator().hasNext())
            {
                return;
            }
        }
        Files.delete(storageRootPath);
    }
    
    @Override
    public RootRepositoryPath getRootDirectory()
    {
        return rootRepositoryPath;
    }

    private Path resolveRootPath()
    {
        String basedir = repository.getBasedir();
        if (StorageProviderEnum.S3.getType().equals(repository.getStorageProvider())) {
            ConfigurationManagementService configurationManagementService = SpringUtil.getBean(ConfigurationManagementService.class);
            String globalS3Bucket = configurationManagementService.getConfiguration().getAdvancedConfiguration().getGlobalS3Bucket();
            if (StringUtils.isNotBlank(globalS3Bucket)) {
                globalS3Bucket = GlobalConstants.SEPARATOR + StringUtils.removeEnd(StringUtils.removeStart(globalS3Bucket, GlobalConstants.SEPARATOR), GlobalConstants.SEPARATOR);
                basedir = globalS3Bucket + basedir;
            }
        }
        Path rootPath = Optional.ofNullable(basedir)
                                .filter(p -> !p.trim().isEmpty())
                                .map(p -> getTarget().getPath(p).toAbsolutePath().normalize())
                                .orElseGet(() -> super.getRootDirectory().resolve(repository.getId()))
                                .toAbsolutePath()
                                .normalize();
        return rootPath;
    }

    public RepositoryPath getTempPath()
    {
        return getRootDirectory().resolve(TEMP).toAbsolutePath();
    }

    @Override
    public RepositoryPath getPath(String first,
                                  String... more)
    {
        return new RepositoryPath(getTarget().getPath(first, more), this);
    }

    public abstract Set<String> getDigestAlgorithmSet();

}
