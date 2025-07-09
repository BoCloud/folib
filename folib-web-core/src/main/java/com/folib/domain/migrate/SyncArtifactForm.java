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
package com.folib.domain.migrate;

import com.folib.components.jfrogArtifactSync.JfrogPropertySyncer;
import com.folib.storage.repository.Repository;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

/**
 * @author veadan
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SyncArtifactForm {

    /**
     * 同步类型
     */
    private String type;

    /**
     * 存储空间
     */
    @NotBlank(message = "存储空间不能为空")
    private String storageId;

    /**
     * 仓库名称
     */
    @NotBlank(message = "仓库名称不能为空")
    private String repositoryId;

    /**
     * 浏览地址
     */
    private String browseUrl;

    /**
     * 休眠毫秒数
     */
    private Integer sleepMillis;

    /**
     * dom
     */
    private String dom;

    /**
     * 每批数量
     */
    private Integer batch;

    /**
     * 索引路径
     */
    private String indexPath;

    /*
        最大线程数
     */
    private Integer maxThreadNum;


    private int totalArtifact;

    private int syncMount;

    private String migrateId;

    private String username;

    private String password;

    private int syncMeta;

    private String apiUrl;

    private JfrogPropertySyncer syncer;

    private Repository repository;


    public String getStoreAndRepo(){
        return this.storageId+":"+repositoryId;
    }


}
