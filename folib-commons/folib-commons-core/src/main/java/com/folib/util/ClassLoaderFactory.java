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

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.springframework.util.Assert;

import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Path;
import java.util.Collection;

/**
 * @author veadan
 */
public final class ClassLoaderFactory
{

    private ClassLoaderFactory()
    {

    }

    @SuppressFBWarnings(value = "DP_CREATE_CLASSLOADER_INSIDE_DO_PRIVILEGED")
    public static URLClassLoader urlClassLoaderFromPaths(ClassLoader parent,
                                                         Collection<Path> paths)
    {
        Assert.notNull(paths, "paths collection cannot be null");

        final URL[] urls = paths.stream().map(Path::toUri)
                                         .map(ThrowingFunction.unchecked(URI::toURL))
                                         .toArray(URL[]::new);

        return new URLClassLoader(urls, parent);
    }
}
