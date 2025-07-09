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
package com.folib.artifact.locator.handlers;

import com.folib.providers.io.RepositoryPath;
import com.folib.providers.layout.LayoutFileSystemProvider;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class ArtifactLocationGenerateChecksumOperation
        extends AbstractArtifactLocationHandler
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactLocationGenerateChecksumOperation.class);

    private Path previousPath;

    private String lastModifiedTime;

    private boolean forceRegeneration = false;

    @Override
    public void execute(RepositoryPath path)
            throws IOException
    {

        RepositoryPath parentPath = path;

        // Don't enter visited paths (i.e. version directories such as 1.2, 1.3, 1.4...)
        if (!getVisitedRootPaths().isEmpty() && getVisitedRootPaths().containsKey(parentPath))
        {
            List<RepositoryPath> visitedVersionPaths = getVisitedRootPaths().get(parentPath);

            if (visitedVersionPaths.contains(path))
            {
                return;
            }
        }

        if (logger.isDebugEnabled())
        {
            // We're using System.out.println() here for clarity and due to the length of the lines
            System.out.println(parentPath);
        }

        // The current directory is out of the tree
        if (previousPath != null && !parentPath.startsWith(previousPath))
        {
            getVisitedRootPaths().remove(previousPath);
            previousPath = parentPath;
        }

        if (previousPath == null)
        {
            previousPath = parentPath;
        }

        RepositoryPath basePath = parentPath;
        LayoutFileSystemProvider provider = (LayoutFileSystemProvider) basePath.getFileSystem()
                                                                                                   .provider();
        provider.storeChecksum(basePath, lastModifiedTime, forceRegeneration);
    }

    public boolean getForceRegeneration()
    {
        return forceRegeneration;
    }

    public void setForceRegeneration(boolean forceRegeneration)
    {
        this.forceRegeneration = forceRegeneration;
    }

    public String getLastModifiedTime() {
        return lastModifiedTime;
    }

    public void setLastModifiedTime(String lastModifiedTime) {
        this.lastModifiedTime = lastModifiedTime;
    }
}
