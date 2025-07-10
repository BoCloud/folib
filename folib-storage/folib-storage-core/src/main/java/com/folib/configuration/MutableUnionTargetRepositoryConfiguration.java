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
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MutableUnionTargetRepositoryConfiguration
        implements Serializable {

    /**
     * 节点
     */
    private String node;

    /**
     * 类型
     */
    private String type = "inner";

    /**
     * 存储空间
     */
    private String storageId;

    /**
     * 仓库名称
     */
    private String repositoryId;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof MutableUnionTargetRepositoryConfiguration)) {
            return false;
        }
        MutableUnionTargetRepositoryConfiguration that = (MutableUnionTargetRepositoryConfiguration) o;
        return node.equals(that.node) &&
                type.equals(that.type) &&
                (!StringUtils.isNotBlank(storageId) || storageId.equals(that.storageId)) &&
                repositoryId.equals(that.repositoryId);
    }

    @Override
    public int hashCode() {
        return StringUtils.isNotBlank(storageId) ? Objects.hash(node, type, storageId, repositoryId) : Objects.hash(node, type, repositoryId);
    }
}