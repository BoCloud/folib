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
package com.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;


/**
 * @author veadan
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "webhook_events_log")
public class WebhookEventsLog implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 主键ID
     */
    @Id
    private Long id;

    /**
     * 事件类型
     */
    @Column(name = "event_type")
    private String eventType;

    /**
     * 事件仓库名称
     */
    @Column(name = "event_repository_id")
    private String eventRepositoryId;

    /**
     * 存储空间名称
     */
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 仓库名称
     */
    @Column(name = "repository_id")
    private String repositoryId;

    /**
     * 制品名称
     */
    @Column(name = "artifact_name")
    private String artifactName;

    /**
     * 制品路径
     */
    @Column(name = "artifact_path")
    private String artifactPath;

    /**
     * 源路径
     */
    @Column(name = "source_artifact_path")
    private String sourceArtifactPath;

    /**
     * 目标路径
     */
    @Column(name = "target_artifact_path")
    private String targetArtifactPath;

    /**
     * sha256的checksum
     */
    @Column(name = "sha256_checksum")
    private String sha256Checksum;

    /**
     * 文件大小（单位：字节）
     */
    @Column(name = "size")
    private Long size;

    /**
     * 访问url前缀
     */
    @Column(name = "base_url")
    private String baseUrl;

    /**
     * 状态（1：初始状态 2：成功 3：失败）
     */
    @Column(name = "status")
    private Integer status;

    /**
     * 失败原因
     */
    @Column(name = "failure_reason")
    private String failureReason;

    /**
     * 重试标记（0:不重试，1:重试）
     */
    @Column(name = "retry")
    private Integer retry;

    /**
     * 重试次数
     */
    @Column(name = "retry_count")
    private Integer retryCount;

    /**
     * 重试时间
     */
    @Column(name = "retry_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date retryTime;

    /**
     * 创建人
     */
    @Column(name = "create_by")
    private String createBy;

    /**
     * 创建时间
     */
    @Column(name = "create_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /**
     * 更新人
     */
    @Column(name = "update_by")
    private String updateBy;

    /**
     * 更新时间
     */
    @Column(name = "update_time")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;

}
