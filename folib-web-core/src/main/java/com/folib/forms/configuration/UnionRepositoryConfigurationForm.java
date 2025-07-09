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
package com.folib.forms.configuration;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.folib.configuration.MutableUnionRepositoryConfiguration;
import com.folib.configuration.MutableUnionTargetRepositoryConfiguration;
import com.folib.configuration.UnionRepositoryConfiguration;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * @author leipenghui
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UnionRepositoryConfigurationForm
        implements Serializable {

    /**
     * 状态 true 启用 false 关闭
     */
    @NotNull(message = "状态不能为空")
    private Boolean enable;
    /**
     * 同步类型 1 制品路径 2 元数据
     */
    @NotNull(message = "同步类型不能为空")
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
    @Valid
    private Set<UnionTargetRepositoryConfigurationForm> unionTargetRepositories;

    public UnionRepositoryConfigurationForm(UnionRepositoryConfiguration unionConfiguration) {
        this.enable = unionConfiguration.getEnable();
        this.syncType = unionConfiguration.getSyncType();
        this.artifactPaths = unionConfiguration.getArtifactPaths();
        this.metadataKey = unionConfiguration.getMetadataKey();
        this.metadataValue = unionConfiguration.getMetadataValue();
        this.unionTargetRepositories = Optional.ofNullable(unionConfiguration.getUnionTargetRepositories()).orElse(new LinkedHashSet<>()).stream().map(item -> {
            UnionTargetRepositoryConfigurationForm unionTargetRepositoryConfigurationForm = UnionTargetRepositoryConfigurationForm.builder().build();
            BeanUtils.copyProperties(item, unionTargetRepositoryConfigurationForm);
            return unionTargetRepositoryConfigurationForm;
        }).collect(Collectors.toCollection(LinkedHashSet::new));
    }

    @JsonIgnore()
    public static UnionRepositoryConfigurationForm fromConfiguration(UnionRepositoryConfiguration source) {
        UnionRepositoryConfiguration unionConfiguration = Optional.ofNullable(source).orElse(
                new UnionRepositoryConfiguration(new MutableUnionRepositoryConfiguration())
        );
        return new UnionRepositoryConfigurationForm(unionConfiguration);
    }

    @JsonIgnore()
    public MutableUnionRepositoryConfiguration getMutableUnionRepositoryConfiguration() {
        MutableUnionRepositoryConfiguration mutableUnionRepositoryConfiguration = MutableUnionRepositoryConfiguration.builder()
                .enable(enable).syncType(syncType).artifactPaths(artifactPaths).metadataKey(metadataKey).metadataValue(metadataValue).build();
        mutableUnionRepositoryConfiguration.setUnionTargetRepositories(Optional.ofNullable(unionTargetRepositories).orElse(new LinkedHashSet<>()).stream().map(item -> {
            MutableUnionTargetRepositoryConfiguration mutableUnionTargetRepositoryConfiguration = MutableUnionTargetRepositoryConfiguration.builder().build();
            BeanUtils.copyProperties(item, mutableUnionTargetRepositoryConfiguration);
            return mutableUnionTargetRepositoryConfiguration;
        }).collect(Collectors.toCollection(LinkedHashSet::new)));
        return mutableUnionRepositoryConfiguration;
    }
}