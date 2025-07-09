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


import com.google.common.collect.Sets;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import javax.annotation.concurrent.Immutable;
import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Immutable
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionRepositoryConfiguration
        implements Serializable {

    /**
     * 状态 true 启用 false 关闭
     */
    private Boolean enable;

    /**
     * 同步类型 1 制品路径 2 元数据
     */
    private Integer syncType;

    /**
     * 制品路径
     */
    private Set<String> artifactPaths;

    /**
     * 元数据key
     */
    private String metadataKey;

    /**
     * 元数据value
     */
    private String metadataValue;

    /**
     * 联邦仓库列表
     */
    private Set<UnionTargetRepositoryConfiguration> unionTargetRepositories;

    public UnionRepositoryConfiguration(MutableUnionRepositoryConfiguration mutableUnionConfiguration) {
        this.enable = mutableUnionConfiguration.getEnable();
        this.syncType = mutableUnionConfiguration.getSyncType();
        this.artifactPaths = mutableUnionConfiguration.getArtifactPaths();
        this.metadataKey = mutableUnionConfiguration.getMetadataKey();
        this.metadataValue = mutableUnionConfiguration.getMetadataValue();
        this.unionTargetRepositories = Optional.ofNullable(mutableUnionConfiguration.getUnionTargetRepositories()).orElse(Sets.newLinkedHashSet()).stream().map(item -> {
            UnionTargetRepositoryConfiguration unionTargetRepositoryConfiguration = UnionTargetRepositoryConfiguration.builder().build();
            BeanUtils.copyProperties(item, unionTargetRepositoryConfiguration);
            return unionTargetRepositoryConfiguration;
        }).collect(Collectors.toCollection(LinkedHashSet::new));
    }

    public Boolean getEnable() {
        return Objects.isNull(enable) ? false : enable;
    }

    public Set<String> getArtifactPaths() {
        return Objects.isNull(artifactPaths) ? Sets.newLinkedHashSet() : artifactPaths;
    }

    public Set<UnionTargetRepositoryConfiguration> getUnionTargetRepositories() {
        return Objects.isNull(unionTargetRepositories) ? Sets.newLinkedHashSet() : unionTargetRepositories;
    }
}