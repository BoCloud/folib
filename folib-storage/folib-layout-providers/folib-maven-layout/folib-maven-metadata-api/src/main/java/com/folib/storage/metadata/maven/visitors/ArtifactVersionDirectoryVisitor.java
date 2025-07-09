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
package com.folib.storage.metadata.maven.visitors;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author stodorov
 */
public class ArtifactVersionDirectoryVisitor
        extends SimpleFileVisitor<Path>
{

    private static final String CHECKSUM_PATTERN = "glob:*.{md5,sha1,sha256,sha512,sm3}";
    private static final String METADATA_PATTERN = "glob:maven-metadata.*";
    private static final String SNAPSHOT_PATTERN = "glob:*SNAPSHOT*";

    private List<Path> matchingPaths = new ArrayList<>();

    private PathMatcher checksumFileMatcher;
    private PathMatcher metadataFileMatcher;
    private PathMatcher snapshotFileMatcher;

    private static final Logger logger = LoggerFactory.getLogger(ArtifactVersionDirectoryVisitor.class);


    public ArtifactVersionDirectoryVisitor()
    {
        checksumFileMatcher = FileSystems.getDefault().getPathMatcher(CHECKSUM_PATTERN);
        metadataFileMatcher = FileSystems.getDefault().getPathMatcher(METADATA_PATTERN);
        snapshotFileMatcher = FileSystems.getDefault().getPathMatcher(SNAPSHOT_PATTERN);
    }

    @Override
    public FileVisitResult visitFile(Path file,
                                     BasicFileAttributes attr)
            throws IOException
    {
        Path name = file.getFileName();
        if (!checksumFileMatcher.matches(name) && !metadataFileMatcher.matches(name))
        {
            if (!snapshotFileMatcher.matches(name))
            {
                matchingPaths.add(file);
            }
            else
            {
                //
                // TODO: Make it possible to configure what should be done when
                //       a snapshot version directory contains both 1.2-SNAPSHOT
                //       and timestamped versions such as 1.2-20150507.013444-1.
                //
                // Current action: Don't add matching file to matched paths and log a warning message
                // Result: Generates metadata as if the directory contains only timestamped artifacts (if any)
                //
                logger.warn("Snapshot artifact name contains SNAPSHOT instead of timestamp: {}", file.toAbsolutePath());
            }
        }

        return FileVisitResult.CONTINUE;
    }

    public List<Path> getMatchingPaths(){
        return matchingPaths;
    }

}
