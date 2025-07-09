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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 * @author stodorov
 */
public class ArtifactLocationReportOperation
        extends AbstractArtifactLocationHandler
{

    private static final Logger logger = LoggerFactory.getLogger(ArtifactLocationReportOperation.class);

    private RepositoryPath previousPath;


    public ArtifactLocationReportOperation()
    {
    }

    public ArtifactLocationReportOperation(RepositoryPath basePath)
    {
        setBasePath(basePath);
    }

    @Override
    public void execute(RepositoryPath path) throws IOException
    {
        List<Path> filePathList;
        try (Stream<Path> pathStream = Files.walk(path)) {
            filePathList =  pathStream
                    .filter(p -> !p.getFileName().startsWith(".pom"))
                    .sorted()
                    .collect(Collectors.toList());

        }
        if (filePathList.isEmpty())
        {
            return;
        }
        // Don't enter visited paths (i.e. version directories such as 1.2, 1.3, 1.4...)
        if (getVisitedRootPaths().containsKey(path) && getVisitedRootPaths().get(path).contains(path))
        {
            return;
        }

        if (logger.isDebugEnabled())
        {
            // We're using System.out.println() here for clarity and due to the length of the lines
            System.out.println(path);
        }

        // The current directory is out of the tree
        if (previousPath != null && !path.startsWith(previousPath))
        {
            getVisitedRootPaths().remove(previousPath);
            previousPath = path;
        }

        if (previousPath == null)
        {
            previousPath = path;
        }

        List<RepositoryPath> versionDirectories = getVersionDirectories(path);
        if (versionDirectories != null)
        {
            getVisitedRootPaths().put(path, versionDirectories);

            System.out.println(path);
        }
    }

}
