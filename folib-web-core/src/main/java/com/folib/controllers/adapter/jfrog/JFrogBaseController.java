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
package com.folib.controllers.adapter.jfrog;

import cn.hutool.extra.spring.SpringUtil;
import com.google.common.collect.Maps;
import com.folib.components.DistributedCacheComponent;
import com.folib.controllers.BaseArtifactController;
import com.folib.storage.Storage;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;

/**
 * @author veadan
 * @date 2023/10/12
 **/
public abstract class JFrogBaseController extends BaseArtifactController {

    private static final String REPOSITORY_NOT_FOUND_MESSAGE = "The %s repository was not found.";

    private static final String NOT_FOUND_MESSAGE = "The %s Artifact was not found.";

    /**
     * 获取设置默认的存储空间
     *
     * @param repositoryId 仓库名称
     * @return 存储空间
     */
    public String getDefaultStorageId(String repositoryId) {
        DistributedCacheComponent distributedCacheComponent = SpringUtil.getBean(DistributedCacheComponent.class);
        if (StringUtils.isNotBlank(repositoryId)) {
            //按照仓库查询对应的存储空间
            String key = "JFrogAdapterStorage_" + repositoryId;
            String jFrogAdapterStorage = distributedCacheComponent.get(key);
            if (StringUtils.isNotBlank(jFrogAdapterStorage)) {
                return jFrogAdapterStorage;
            }
        }
        String key = "JFrogAdapterDefaultStorage";
        String jFrogAdapterDefaultStorage = distributedCacheComponent.get(key);
        if (StringUtils.isBlank(jFrogAdapterDefaultStorage)) {
            throw new RuntimeException("Default storage not found,Please Set the default storageId");
        }
        return jFrogAdapterDefaultStorage;
    }

    /**
     * 校验存储空间和仓库是否存在
     *
     * @param storageId    存储空间
     * @param repositoryId 仓库名称
     * @return 结果
     */
    public boolean checkRepository(String storageId, String repositoryId) {
        Storage storage = getStorage(storageId);
        if (Objects.isNull(storage)) {
            return false;
        }
        if (Objects.isNull(storage.getRepository(repositoryId))) {
            return false;
        }
        return true;
    }

    /**
     * 仓库不存在
     *
     * @param type 源仓库 或者 目标仓库 可以为空
     */
    public ResponseEntity<Object> repositoryNotFound(String type) {
        if (StringUtils.isBlank(type)) {
            type = "";
        }
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, String.format(REPOSITORY_NOT_FOUND_MESSAGE, type)));
    }

    /**
     * 仓库不存在
     */
    public ResponseEntity<Object> repositoryNotFound() {
        return repositoryNotFound("");
    }

    /**
     * 制品不存在
     *
     * @param artifact 制品
     */
    public ResponseEntity<Object> artifactNotFound(String artifact) {
        return ResponseEntity.status(HttpStatus.NOT_FOUND).body(handlerErrors(null, String.format(NOT_FOUND_MESSAGE, artifact)));
    }

    /**
     * 制品不存在
     */
    public ResponseEntity<Object> artifactNotFound() {
        return artifactNotFound("");
    }

    /**
     * 处理错误
     *
     * @param status  状态
     * @param message 消息
     * @return 结果
     */
    public Map<String, Object> handlerErrors(Integer status, String message) {
        Map<String, Object> result = Maps.newHashMap();
        Map<String, Object> resultData = Maps.newHashMap();
        if (Objects.isNull(status)) {
            status = HttpStatus.NOT_FOUND.value();
        }
        resultData.put("status", status);
        if (StringUtils.isBlank(message)) {
            message = NOT_FOUND_MESSAGE;
        }
        resultData.put("message", message);
        result.put("errors", Collections.singletonList(resultData));
        return result;
    }
}
