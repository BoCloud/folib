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
package com.folib.io;

import java.io.IOException;
import java.nio.file.FileStore;
import java.nio.file.FileSystem;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.WatchService;
import java.nio.file.attribute.UserPrincipalLookupService;
import java.nio.file.spi.FileSystemProvider;
import java.util.ArrayList;
import java.util.Optional;
import java.util.Set;

import com.folib.booters.PropertiesBooter;
import com.folib.providers.storage.StorageProvider;
import com.folib.storage.Storage;

/**
 * This class decoretes storage {@link FileSystem} implementation.
 *
 * @author @author veadan
 * 
 * @see StorageProvider
 */
public abstract class StorageFileSystem
        extends FileSystem
{

    private final PropertiesBooter propertiesBooter;
    
    private final FileSystem target;
    
    private final Storage storage;

    public StorageFileSystem(Storage storage, PropertiesBooter propertiesBooter, FileSystem target)
    {
        this.target = target;
        this.storage = storage;
        this.propertiesBooter = propertiesBooter;
    }

    public FileSystem getTarget()
    {
        return target;
    }

    public FileSystemProvider provider()
    {
        return target.provider();
    }

    public void close()
            throws IOException
    {
        target.close();
    }

    public boolean isOpen()
    {
        return target.isOpen();
    }

    public boolean isReadOnly()
    {
        return target.isReadOnly();
    }

    public String getSeparator()
    {
        return target.getSeparator();
    }

    public Iterable<Path> getRootDirectories()
    {
        ArrayList<Path> result = new ArrayList<Path>();
        result.add(getRootDirectory());
        return result;
    }

    public Path getRootDirectory() {
        String basedir = storage.getBasedir();

        Path storagesRoot = Optional.ofNullable(propertiesBooter.getStorageBooterBasedir())
                                    .filter(p -> !p.trim().isEmpty())
                                    .map(p -> getTarget().getPath(p))
                                    .orElseGet(() -> getTarget().getPath(propertiesBooter.getVaultDirectory(),
                                                                         "/storages"))
                                    .toAbsolutePath()
                                    .normalize();
        return Optional.ofNullable(basedir)
                .filter(p -> !p.trim().isEmpty())
                .map(p -> getTarget().getPath(propertiesBooter.getVaultDirectory() + p).toAbsolutePath().normalize())
                .orElseGet(() -> storagesRoot.resolve(storage.getId())).toAbsolutePath().normalize();
    }

    public Iterable<FileStore> getFileStores()
    {
        return target.getFileStores();
    }

    public Set<String> supportedFileAttributeViews()
    {
        return target.supportedFileAttributeViews();
    }

    public Path getPath(String first,
                        String... more)
    {
        return target.getPath(first, more);
    }

    public PathMatcher getPathMatcher(String syntaxAndPattern)
    {
        return target.getPathMatcher(syntaxAndPattern);
    }

    public UserPrincipalLookupService getUserPrincipalLookupService()
    {
        return target.getUserPrincipalLookupService();
    }

    public WatchService newWatchService()
            throws IOException
    {
        return target.newWatchService();
    }

}
