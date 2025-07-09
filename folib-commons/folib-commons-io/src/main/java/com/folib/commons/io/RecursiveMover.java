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
package com.folib.commons.io;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.EnumSet;

import static java.nio.file.FileVisitResult.CONTINUE;
import static java.nio.file.FileVisitResult.SKIP_SIBLINGS;
import static java.nio.file.StandardCopyOption.COPY_ATTRIBUTES;
import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;

/**
 * @author veadan
 */
public class RecursiveMover
        implements FileVisitor<Path>
{

    private static final Logger logger = LoggerFactory.getLogger(RecursiveMover.class);

    private final Path source;

    private final Path target;


    public RecursiveMover(Path source,
                          Path target)
    {
        this.source = source;
        this.target = target;
    }

    public FileVisitResult preVisitDirectory(Path sourcePath,
                                             BasicFileAttributes attrs)
            throws IOException
    {
        Path relativePath = relativizeTargetPath(sourcePath);
        Path targetPath = target.resolve(relativePath);

        if (Files.exists(sourcePath) && Files.exists(targetPath))
        {
            // 1) If its a file, delete it
            if (Files.isRegularFile(sourcePath))
            {
                Files.deleteIfExists(sourcePath);
            }
            // 2) If its a directory, iterate and check
            else
            {
                // Carry out a directory move
                String[] paths = sourcePath.toFile().list();
                if (paths != null)
                {
                    Arrays.sort(paths);

                    for (String path : paths)
                    {
                        Path srcPath = sourcePath.resolve(path);
                        Path destPath = targetPath.resolve(path);

                        if (Files.isDirectory(srcPath))
                        {
                            if (Files.notExists(targetPath))
                            {
                                // Make sure we've created the destination directory:

                                move(sourcePath, targetPath, true);
                            }
                            else
                            {
                                EnumSet<FileVisitOption> opts = EnumSet.of(FileVisitOption.FOLLOW_LINKS);
                                RecursiveMover recursiveMover = new RecursiveMover(srcPath, destPath.getParent());
                                Files.walkFileTree(srcPath, opts, Integer.MAX_VALUE, recursiveMover);
                            }
                        }
                        else
                        {
                            Files.move(srcPath, destPath, REPLACE_EXISTING);
                        }
                    }

                    if (paths.length == 0)
                    {
                        // Make sure the source directory has been removed, if its empty.
                        // This is for cases where the destination contains the directory or part of the resources.
                        Files.deleteIfExists(sourcePath);
                    }
                }

                return SKIP_SIBLINGS;
            }
        }

        return CONTINUE;
    }

    private Path relativizeTargetPath(Path dir)
    {
        return Paths.get(source.toFile().getName() + "/" + source.relativize(dir.toAbsolutePath()));
    }

    public FileVisitResult visitFile(Path sourcePath,
                                     BasicFileAttributes attrs)
    {
        Path relativePath = relativizeTargetPath(sourcePath);
        Path targetPath = target.resolve(relativePath);

        if (Files.notExists(targetPath.getParent()))
        {
            move(sourcePath.getParent(), targetPath.getParent(), false);

            return SKIP_SIBLINGS;
        }
        else
        {
            move(sourcePath, targetPath, false);

            return CONTINUE;
        }
    }

    public FileVisitResult postVisitDirectory(Path dir,
                                              IOException exc)
    {
        return CONTINUE;
    }

    public FileVisitResult visitFileFailed(Path file,
                                           IOException e)
    {
        if (e instanceof FileSystemLoopException)
        {
            logger.error("Cycle detected: {}", file);
        }
        else
        {
            logger.error("Unable to move: {}", file, e);
        }

        return CONTINUE;
    }

    public void move(Path source,
                     Path target,
                     boolean preserve)
    {
        CopyOption[] options = (preserve) ?
                               new CopyOption[]{ COPY_ATTRIBUTES,
                                                 REPLACE_EXISTING } :
                               new CopyOption[]{ REPLACE_EXISTING };

        try
        {
            Files.move(source, target, options);
        }
        catch (FileAlreadyExistsException e)
        {
            // Ignore
            logger.error("File already exists", e);
        }
        catch (IOException e)
        {
            logger.error("ERROR: Unable to move {} to {}!",
                         source.toAbsolutePath(),
                         target.toAbsolutePath(), e);
        }
    }

}

