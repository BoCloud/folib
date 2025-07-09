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
package com.folib.storage.indexing;

import com.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.nio.file.Files;

import org.apache.lucene.search.IndexSearcher;
import org.apache.maven.index.context.IndexingContext;
import org.apache.maven.index.incremental.DefaultIncrementalHandler;
import org.apache.maven.index.packer.DefaultIndexPacker;
import org.apache.maven.index.packer.IndexPackingRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class IndexPacker
{

    private static final Logger logger = LoggerFactory.getLogger(IndexPacker.class);

    private static final org.apache.maven.index.packer.IndexPacker INSTANCE = new DefaultIndexPacker(
            new DefaultIncrementalHandler());

    public static RepositoryPath pack(final RepositoryPath indexPath,
                                      final IndexingContext context)
            throws IOException
    {
        final IndexSearcher indexSearcher = context.acquireIndexSearcher();
        try
        {

            final IndexPackingRequest request = new IndexPackingRequest(context,
                                                                        indexSearcher.getIndexReader(),
                                                                        indexPath.toFile());
            request.setUseTargetProperties(true);
            IndexPacker.INSTANCE.packIndex(request);

            logger.info("Index for {} was packed successfully.", indexPath);
        }
        finally
        {
            context.releaseIndexSearcher(indexSearcher);
        }
        return indexPath.resolve(IndexingContext.INDEX_FILE_PREFIX + ".gz");
    }

    public static boolean packageExists(final RepositoryPath indexPath)
    {
        return Files.exists(indexPath.resolve(IndexingContext.INDEX_FILE_PREFIX + ".gz"));
    }
}
