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
import lombok.Data;
import lombok.experimental.Accessors;
import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.sql.Timestamp;

@Data
@Accessors(chain = true)
@Table(name = "folib_scanner")
@ApiModel("folib_scanner")
public class FolibLock implements Serializable {

    @Id
    @ApiModelProperty("锁的路径名称")
    @Column(name = "name")
    private String name;

    @ApiModelProperty("锁定直到")
    @Column(name = "lock_until")
    private Timestamp lockUntil;

    @ApiModelProperty("锁定的时间")
    @Column(name = "lock_at")
    private Timestamp lockedAt;

    @ApiModelProperty("锁定者")
    @Column(name = "locked_by")
    private String lockedBy;


}
