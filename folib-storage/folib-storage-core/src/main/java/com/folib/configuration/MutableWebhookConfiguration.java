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
package com.folib.configuration;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Objects;
import java.util.Set;

/**
 * @author veadan
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MutableWebhookConfiguration
        implements Serializable {

    /**
     * uuid
     */
    private String uuid;
    /**
     * url
     */
    private String url;

    /**
     * 访问令牌
     */
    private String accessToken;

    //事件仓库 storageId:repositoryId
    private Set<String>  repository;

    /**
     * 触发事件
     */
    private Set<String> events;
    /**
     * 启用ssl true 启用 false 不启用
     */
    private Boolean ssl;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MutableWebhookConfiguration)) {
            return false;
        }
        MutableWebhookConfiguration that = (MutableWebhookConfiguration) o;
        return uuid.equals(that.uuid);
    }

    @Override
    public int hashCode() {
        return Objects.hash(uuid);
    }
}
