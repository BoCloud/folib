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

import com.folib.domain.Artifact;

import java.io.IOException;
import java.nio.file.Files;

/**
 * The main concept of {@link TempRepositoryPath} is to provide atomacity into
 * artifact files store process. Files stored into temporary location first,
 * along with all additional logic needed, and, if procedure completed
 * successfully, then file just moved into original location, other way
 * "transaction" will be rolled back and temporary file will be removed.
 *
 * @see StorageFileSystemProvider.TempOutputStream
 * @author veadan
 *
 */
public class TempRepositoryPath extends RepositoryPath
{

    private RepositoryPath tempTarget;

    private TempRepositoryPath(RepositoryPath tempPath)
    {
        super(tempPath.getTarget(), tempPath.getFileSystem());
    }

    public RepositoryPath getTempTarget()
    {
        return tempTarget;
    }

    public static TempRepositoryPath of(RepositoryPath path)
        throws IOException
    {
        RepositoryPath tempPathBase = path.getFileSystem().getTempPath();
        RepositoryPath tempPath = StorageFileSystemProvider.rebase(path, tempPathBase);

        if (!Files.exists(tempPath.getParent().getTarget()))
        {
            Files.createDirectories(tempPath.getParent().getTarget());
        }

        TempRepositoryPath result = new TempRepositoryPath(tempPath);
        result.tempTarget = path;
        result.artifact = path.artifact;

        return result;
    }
    
    @Override
    public Artifact getArtifactEntry()
        throws IOException
    {
        return tempTarget.getArtifactEntry();
    }

    @Override
    public Boolean getArtifactExist() throws IOException
    {
        return tempTarget.getArtifactExist();
    }
}
