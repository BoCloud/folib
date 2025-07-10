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

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * @author veadan
 * @since 2024-12-31 20:36
 */
@Data
@Table(name = "migrate_info")
public class MigrateInfo {

    @Id
    @GeneratedValue(generator = "JDBC", strategy = GenerationType.IDENTITY)
    @ApiModelProperty("id")
    @Column(name = "id")
    private Long id;
    /**
     * 存储空间
     */
    @Column(name = "storage_id")
    private String storageId;

    /**
     * 仓库
     */
    @Column(name = "repository_id")
    private String repositoryId;

    @Column(name = "layout")
    private String layout;

    /**
     * 修改布局
     */
    @Column(name = "post_layout")
    private String postLayout;

    /**
     * 迁移id
     */
    @Column(name = "migrate_id")
    private String migrateId;

    /**
     * jfrog
     */
    @Column(name = "migrate_type")
    private String migrateType;

    /**
     * 迁移状态
     */
    @Column(name = "sync_status")
    private Integer syncStatus;

    /**
     * 制品总数
     */
    @Column(name = "total_artifact")
    private Integer totalArtifact;

    /**
     * 迁移成功量
     */
    @Column(name = "success_mount")
    private Integer successMount;

    /**
     * 原制品大小
     */
    @Column(name = "used_space")
    private String usedSpace;

    @Column(name = "sync_dir_path")
    private String syncDirPath;

    @Column(name = "sync_property")
    private Integer syncProperty;

    @Column(name = "index_finish")
    private Integer indexFinish;


    public String getStorageIdAndRepositoryId(){
        return this.storageId+":"+this.repositoryId;
    }


}
