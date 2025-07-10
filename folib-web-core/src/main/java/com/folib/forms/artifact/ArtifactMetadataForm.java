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
package com.folib.forms.artifact;

import com.folib.enums.ArtifactMetadataEnum;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author leipenghui
 * @date 2022/11/29
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArtifactMetadataForm {

    /**
     * 元数据类型
     *
     * @see ArtifactMetadataEnum
     */
    @NotBlank(message = "请填写元数据类型", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    private String type;
    /**
     * 前端是否展示
     */
    @NotNull(message = "请选择前端是否展示", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    @Min(value = 0, message = "前端是否展示仅为0或1", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    @Max(value = 1, message = "前端是否展示仅为0或1", groups = {ConfigurationAddOrUpdateGroup.class, AddOrUpdateGroup.class})
    private Integer viewShow;
    /**
     * 元数据key
     */
    @NotBlank(message = "请填写元数据key", groups = {ConfigurationAddOrUpdateGroup.class, ConfigurationDeleteGroup.class, AddOrUpdateGroup.class, DeleteGroup.class})
    private String key;
    /**
     * 元数据值
     */
    @NotBlank(message = "请填写元数据值", groups = {AddOrUpdateGroup.class})
    private String value;
    /**
     * 展示位置
     */
    private String location;
    /**
     * 存储空间名称
     */
    @NotBlank(message = "存储空间名称不能为空", groups = {AddOrUpdateGroup.class, DeleteGroup.class})
    private String storageId;
    /***
     * 仓库名称
     */
    @NotBlank(message = "仓库名称不能为空", groups = {AddOrUpdateGroup.class, DeleteGroup.class})
    private String repositoryId;
    /**
     * 制品路径
     */
    @NotBlank(message = "制品路径不能为空", groups = {AddOrUpdateGroup.class, DeleteGroup.class})
    private String artifactPath;

    private Boolean recursive;

    public interface ConfigurationAddOrUpdateGroup
            extends Serializable {
        // 配置新增组
    }

    public interface ConfigurationDeleteGroup
            extends Serializable {
        // 配置删除组
    }

    public interface AddOrUpdateGroup
            extends Serializable {
        // 新增组
    }

    public interface DeleteGroup
            extends Serializable {
        // 删除组
    }
}
