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
package com.folib.artifact.locator;

import com.folib.artifact.locator.handlers.ArtifactDirectoryOperation;
import com.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class ArtifactDirectoryLocator
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactDirectoryLocator.class);

    private ArtifactDirectoryOperation operation;

    /**
     * The basedir to start analyzing from. Define this only, if you need to pass null values for operation.storage and
     * operation.repository.
     */
    private RepositoryPath basedir;

    public void locateArtifactDirectories()
        throws IOException
    {
        long startTime = System.currentTimeMillis();

        RepositoryPath startingPath = getStartingPath();
        if (!Files.exists(startingPath)) {
            logger.info("RepositoryPath [{}] not exist", startingPath.toString());
            return;
        }
        try (Stream<Path> pathStream = Files.walk(startingPath))
        {
            pathStream.filter(Files::isDirectory)
                      // Skip directories which start with a dot (like, for example: .index)
                      .filter(path -> !path.getFileName().toString().startsWith("."))
                      // Note: Sorting can be expensive:
                      .sorted()
                      .forEach(this::execute);
        }

        long endTime = System.currentTimeMillis();

        logger.info("Executed (cache: {}) visits in {} ms.",
                     -operation.getVisitedRootPaths().size(), (endTime - startTime));

        getOperation().getVisitedRootPaths().clear();
    }

    public RepositoryPath getStartingPath()
    {
        // The root path
        RepositoryPath rootPath =
                basedir != null ? basedir : getOperation().getBasePath().getFileSystem().getRootDirectory();
        if (getOperation().getBasePath() != null)
        {
            rootPath = rootPath.resolve(getOperation().getBasePath().relativize());
        }
        rootPath = rootPath.normalize();

        logger.info("ArtifactDirectoryLocator started in: path-[{}]", rootPath);

        return Files.isDirectory(rootPath) ? rootPath : rootPath.getParent();
    }

    public ArtifactDirectoryOperation getOperation()
    {
        return operation;
    }

    public void setOperation(ArtifactDirectoryOperation operation)
    {
        this.operation = operation;
    }

    public RepositoryPath getBasedir()
    {
        return basedir;
    }

    public void setBasedir(RepositoryPath basedir)
    {
        this.basedir = basedir;
    }

    void execute(Path path)
    {
        try
        {
            operation.execute((RepositoryPath) path);
        }
        catch (IOException e)
        {
            logger.error("Failed to execute operation [{}]", operation.getClass().getSimpleName(), e);
        }
    }
    
}
