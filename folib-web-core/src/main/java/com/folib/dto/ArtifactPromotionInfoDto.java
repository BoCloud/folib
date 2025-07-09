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
package com.folib.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.folib.constant.ArtifactSyncRecordStatusEnum;
import com.folib.enums.ArtifactSyncRecordOpsTypeEnum;
import com.folib.enums.ArtifactSyncRecordSyncModelEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import java.util.Date;

/**
 * @author veadan
 * @date 2023/10/12 14:35
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArtifactPromotionInfoDto 
{
    @Id
    @GeneratedValue(generator = "JDBC",strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    private Long id;

    /**
     * 源制品路径
     */
    @ApiModelProperty("源制品路径")
    private String sourcePath;
    /**
     * 目标制品路径
     */
    @ApiModelProperty("目标制品路径")
    private String targetPath;
    /**
     * 制品操作（1：制品晋级；2：制品分发）
     * {@linkplain ArtifactSyncRecordOpsTypeEnum }
     */
    @ApiModelProperty("制品操作（1：制品晋级；2：制品分发）")
    private Integer opsType;
    /**
     * 制品同步编号
     */
    @ApiModelProperty("制品同步编号")
    private String syncNo;
    /**
     * 同步模式（1：推；2：拉）
     * {@linkplain  ArtifactSyncRecordSyncModelEnum }
     */
    @ApiModelProperty("同步模式（1：推；2：拉）")
    private Integer syncModel;
    /**
     * 同步状态（1：就绪；2：同步中；3：成功；4：失败）
     * {@linkplain ArtifactSyncRecordStatusEnum }
     */
    @ApiModelProperty("同步状态（1：就绪；2：同步中；3：成功；4：失败）")
    private Integer status;
    /**
     * 失败的原因
     */
    @ApiModelProperty("失败的原因")
    private String failedReason;
    /**
     * 创建人
     */
    @ApiModelProperty("创建人")
    private String createdBy;
    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createdTime;
    /**
     * 更新人
     */
    @ApiModelProperty("更新人")
    private String updatedBy;
    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updatedTime;
}
