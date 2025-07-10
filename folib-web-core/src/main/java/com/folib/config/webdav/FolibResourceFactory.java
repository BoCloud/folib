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
package com.folib.config.webdav;

import cn.hutool.extra.spring.SpringUtil;
import io.milton.http.ResourceFactory;
import io.milton.http.exceptions.BadRequestException;
import io.milton.http.exceptions.NotAuthorizedException;
import io.milton.resource.Resource;
import lombok.extern.slf4j.Slf4j;

/**
 * @author veadan
 * @since 2025-03-09 16:17
 */
@Slf4j
public class FolibResourceFactory implements ResourceFactory {


    private final FileStorageService storageService;

    public FolibResourceFactory() {
        this.storageService = SpringUtil.getBean(FileStorageService.class);
    }

    @Override
    public Resource getResource(String host, String path) throws NotAuthorizedException, BadRequestException {
        String cleanedPath = path.replaceAll("/{2,}", "/")
                .replaceFirst("^/dav/?", "")
                .replaceAll("/$", "");
        log.info("Original: " + path + " -> Cleaned: " + cleanedPath);
        if (!storageService.exists(cleanedPath)) {
            return null;
        }
        return storageService.isDirectory(cleanedPath) ?
                new FolibFolderResource(cleanedPath, storageService) :
                new FolibFileResource(cleanedPath, storageService);

    }
}
