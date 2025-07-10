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
package com.folib.storage.indexing.remote;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.text.MessageFormat;

import com.google.common.io.Closeables;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.maven.index.updater.ResourceFetcher;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class IndexResourceFetcher
        implements ResourceFetcher, Closeable
{

    private static final String INDEX_URI_PATTERN = "{0}/.index/{1}";

    private static final Logger logger = LoggerFactory.getLogger(IndexResourceFetcher.class);

    private final String repositoryBaseUrl;

    private final CloseableHttpClient client;

    private CloseableHttpResponse response;

    public IndexResourceFetcher(String repositoryBaseUrl,
                                CloseableHttpClient client)
    {
        this.repositoryBaseUrl = StringUtils.removeEnd(repositoryBaseUrl, "/");
        this.client = client;
    }

    @Override
    public void connect(String indexContextId,
                        String indexUpdateUrl)
    {
        // ignored
    }

    @Override
    public void disconnect()
            throws IOException
    {
        close();
    }

    @Override
    public InputStream retrieve(String indexName)
            throws IOException
    {
        final String uri = MessageFormat.format(INDEX_URI_PATTERN, repositoryBaseUrl, indexName);

        logger.info("Getting {}...", uri);

        InputStream result = null;

        response = client.execute(new HttpGet(uri));

        HttpEntity httpEntity = response.getEntity();
        if (httpEntity != null)
        {
            result = httpEntity.getContent();
        }
        return result;
    }

    @Override
    public void close()
            throws IOException
    {
        Closeables.close(response, true);
        Closeables.close(client, true);
    }
}
