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

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;



import java.io.Serializable;
import java.util.Date;

 /**
 * 角色用户关联表;
 * @author veadan
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "角色用户关联表",description = "")
@TableName("role_user_ref")
public class RoleUserRef implements Serializable,Cloneable {
     /**
      * 主键
      */
     @TableId
     @ApiModelProperty(name = "主键", notes = "")
     private Long id;
     /**
      * 角色id
      */
     @ApiModelProperty(name = "角色id", notes = "")
     private String roleId;
     /**
      * 对象id
      */
     @ApiModelProperty(name = "对象id", notes = "")
     private String entityId;
     /**
      * 关联类型;[1-用户id、2-用户组id、3-资源id、4-仓库id]
      */
     @ApiModelProperty(name = "关联类型", notes = "[1-用户id、2-用户组id、3-资源id、4-仓库id]")
     private String refType;
     /**
      * 创建人
      */
     @ApiModelProperty(name = "创建人", notes = "")
     private String createBy;
     /**
      * 创建时间
      */
     @ApiModelProperty(name = "创建时间", notes = "")
     @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
     private Date createTime;
 }