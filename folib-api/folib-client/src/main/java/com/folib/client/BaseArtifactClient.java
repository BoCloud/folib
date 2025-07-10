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
package com.folib.client;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;

/**
 * Implements basic API for artifact processing. Subclasses may specify particular remote method implementations.
 * @author veadan
 */
public abstract class BaseArtifactClient
        implements IArtifactClient {

    protected final Logger logger = LoggerFactory.getLogger(getClass().getName());

    @Override
    public InputStream getResource(String path) {
        return getResource(path, 0);
    }

    protected String escapeUrl(String path) {
        String baseUrl = getContextBaseUrl() + (getContextBaseUrl().endsWith("/") ? "" : "/");
        String p = (path.startsWith("/") ? path.substring(1, path.length()) : path);

        return baseUrl + p;
    }

    protected abstract void put(InputStream is,
                                String url,
                                String fileName,
                                String mediaType)
            throws ArtifactOperationException;
}
