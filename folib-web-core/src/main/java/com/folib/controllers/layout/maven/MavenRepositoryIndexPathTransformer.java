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
package com.folib.controllers.layout.maven;

import com.folib.storage.indexing.IndexTypeEnum;
import com.folib.storage.repository.Repository;

import java.util.function.Function;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.apache.maven.index.context.IndexingContext;

/**
 * We need to support .index/nexus-maven-repository-index.XXX.gz requests
 * but the real path is under `local` or `remote` sub-path
 *
 * @author veadan
 */
public class MavenRepositoryIndexPathTransformer
        implements Function<String, String>
{

    private static final String INDEX_BASE_PATH = ".index/";

    private static final String INDEX_LOCAL_BASE_PATH = INDEX_BASE_PATH + IndexTypeEnum.LOCAL.getType() + "/";

    private static final String INDEX_REMOTE_BASE_PATH = INDEX_BASE_PATH + IndexTypeEnum.REMOTE.getType() + "/";

    private static final Pattern INDEX_FILE_PATTERN = Pattern.compile(
            "^" + INDEX_BASE_PATH + "(" + IndexingContext.INDEX_FILE_PREFIX + "(\\.[0-9]+)?" + ".gz|" +
            IndexingContext.INDEX_REMOTE_PROPERTIES_FILE + ")$");

    private final Repository repository;

    MavenRepositoryIndexPathTransformer(final Repository repository)
    {
        this.repository = repository;
    }

    @Override
    public String apply(final String requestedPath)
    {
        String path = requestedPath;

        if (INDEX_FILE_PATTERN.matcher(path).matches())
        {
            path = repository.isProxyRepository() ? INDEX_REMOTE_BASE_PATH : INDEX_LOCAL_BASE_PATH;
            path += StringUtils.substringAfterLast(requestedPath, INDEX_BASE_PATH);
        }
        return path;
    }
}
