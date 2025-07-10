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
package com.folib.index;

import com.google.common.collect.ArrayListMultimap;
import com.folib.utils.MlModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MlModelRemoteEtagIndex {

    private static final Logger log = LoggerFactory.getLogger(MlModelRemoteEtagIndex.class);


    public MlModelRemoteEtagIndex() {
    }

    public void indexEtagOnArtifact( String storageId, String repositoryId,  String path,  String etag)  {
        if (repositoryId == null) {
            throw new NullPointerException("repositoryId is marked non-null but is null");
        }
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        if (etag == null) {
            throw new NullPointerException("etag is marked non-null but is null");
        }
        ArrayListMultimap<Object, Object> arrayListMultimap = ArrayListMultimap.create();
        arrayListMultimap.put("huggingfaceml.etag.file", MlModelUtils.removeQuote(etag));
        try {
            log.debug("About to try to set attribute {} with value: {}  on repo: {} path {}", "huggingfaceml.etag.file", etag, repositoryId, path );
            // TODO: 2024/6/20
            //this.securityService.callAsSystem(() -> {
            //    this.repositoryService.setAttributes(repoKey, path, attributes);
            //    return null;
            //});
        } catch (Exception e) {
            log.error("Could not set the attribute {} with value: {} on repo: {} path {} message:{}",  "huggingfaceml.etag.file", etag, repositoryId, path, e
                    .getMessage() );
            log.debug("Could not set the attribute {} with value: {} on repo: {} path {}",  "huggingfaceml.etag.file", etag, repositoryId, path, e );
        }
    }
}

