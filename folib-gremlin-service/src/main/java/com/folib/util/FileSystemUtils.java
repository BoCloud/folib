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
package com.folib.util;

import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

import org.springframework.util.Assert;

/**
 * @author veadan
 * @see org.springframework.util.FileSystemUtils
 */
public final class FileSystemUtils
{

    private FileSystemUtils()
    {
    }

    /**
     * This method is almost a mirror of springframework implementation.
     * However, it contains a workaround for {@link ProviderMismatchException}
     * when I trying to .resolve() a Path against another type of Path
     *
     * @see org.springframework.util.FileSystemUtils#copyRecursively(java.nio.file.Path, java.nio.file.Path)
     */
    public static void copyRecursively(Path src,
                                       Path dest)
            throws IOException
    {
        Assert.notNull(src, "Source Path must not be null");
        Assert.notNull(dest, "Destination Path must not be null");
        BasicFileAttributes srcAttr = Files.readAttributes(src, BasicFileAttributes.class);

        if (srcAttr.isDirectory())
        {
            Files.walkFileTree(src, new SimpleFileVisitor<Path>()
            {
                @Override
                public FileVisitResult preVisitDirectory(Path dir,
                                                         BasicFileAttributes attrs)
                        throws IOException
                {
                    Files.createDirectories(dest.resolve(src.relativize(dir).toString()));
                    return FileVisitResult.CONTINUE;
                }

                @Override
                public FileVisitResult visitFile(Path file,
                                                 BasicFileAttributes attrs)
                        throws IOException
                {
                    Files.copy(file, dest.resolve(src.relativize(file).toString()),
                               StandardCopyOption.REPLACE_EXISTING);
                    return FileVisitResult.CONTINUE;
                }
            });
        }
        else if (srcAttr.isRegularFile())
        {
            Files.copy(src, dest);
        }
        else
        {
            throw new IllegalArgumentException("Source File must denote a directory or file");
        }
    }
}
