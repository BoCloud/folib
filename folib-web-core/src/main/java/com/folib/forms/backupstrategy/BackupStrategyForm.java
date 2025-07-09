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
package com.folib.forms.backupstrategy;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.Valid;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.List;

/**
 * @author leipenghui
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("BlockStrategyForm")
public class BackupStrategyForm implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private String id;

    /**
     * 是否启用 1 启用 0 不启用
     */
    @ApiModelProperty("是否启用 1 启用 0 不启用")
    private Boolean enabled;

    /**
     * 备份策略名称
     */
    @ApiModelProperty("备份策略名称")
    @NotBlank(message = "请填写备份策略名称", groups = {SaveGroup.class, UpdateGroup.class, DeleteGroup.class, ExecuteGroup.class})
    private String strategyName;

    /**
     * 模糊备份策略名称
     */
    private String matchStrategyName;

    /**
     * cron定时设置
     */
    @ApiModelProperty("cron定时设置")
    @NotBlank(message = "请填写cron定时设置", groups = {SaveGroup.class, UpdateGroup.class})
    private String cronExpression;

    /**
     * 备份路径
     */
    @ApiModelProperty("备份路径")
    @NotBlank(message = "请填写备份路径", groups = {SaveGroup.class, UpdateGroup.class})
    private String backupPath;

    /**
     * 增量备份 1 是 0 否
     */
    @ApiModelProperty("增量备份 1 是 0 否")
    private Boolean incremental;

    /**
     * 全量备份保留期限
     */
    @ApiModelProperty("全量备份保留期限")
    private Integer retentionPeriod;

    /**
     * 储存空间名称
     */
    private String storageId;

    /**
     * 仓库名称
     */
    private String repositoryId;

    /**
     * 模糊储存空间名称
     */
    private String matchStorageId;

    /**
     * 模糊仓库名称
     */
    private String matchRepositoryId;

    /**
     * 仓库列表
     */
    @ApiModelProperty("仓库列表")
    @Valid
    @NotEmpty(message = "请传入仓库列表", groups = {SaveGroup.class, UpdateGroup.class})
    private List<String> repositories;

    public interface SaveGroup
            extends Serializable {
        // 新增组
    }

    public interface UpdateGroup
            extends Serializable {
        // 更新组
    }

    public interface DeleteGroup
            extends Serializable {
        // 删除组
    }

    public interface ExecuteGroup
            extends Serializable {
        // 执行组
    }
}
