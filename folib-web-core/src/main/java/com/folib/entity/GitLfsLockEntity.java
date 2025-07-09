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


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.*;
import java.io.Serializable;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Table(name = "git_lfs_locks")
@ApiModel("GitLfsLock")
public class GitLfsLockEntity implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @Id
    @ApiModelProperty(name = "id", notes = "")
    private String id;
    /**
     * 存储ID
     */
    @ApiModelProperty(name = "存储ID", notes = "")
    private String storageId;
    /**
     * 仓库ID
     */
    @ApiModelProperty(name = "仓库ID", notes = "")
    private String repositoryId;
    /**
     * 锁定文件的路径
     */
    @ApiModelProperty(name = "锁定文件的路径", notes = "")
    private String path;
    /**
     * 创建锁的时间戳
     */
    @ApiModelProperty(name = "创建锁的时间戳", notes = "")
    private Long lockedAt;
    /**
     * 所属人
     */
    @ApiModelProperty(name = "所属人", notes = "")
    private String owner;
    /**
     * 描述锁所属的服务器引用
     */
    @ApiModelProperty(name = "描述锁所属的服务器引用", notes = "")
    private String ref;

}
