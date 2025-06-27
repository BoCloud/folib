package com.veadan.folib.entity;

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
 * 资源表;
 * @author veadan
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "资源表",description = "")
@TableName("resource")
public class Resource implements Serializable,Cloneable {
     /**
      * 主键
      */
     @ApiModelProperty(name = "主键", notes = "")
     private String id;
     /**
      * api权限
      */
     @ApiModelProperty(name = "api权限", notes = "")
     private String apiAuthoritie;
     /**
      * 存储空间id
      */
     @ApiModelProperty(name = "存储空间id", notes = "")
     private String storageId;
     /**
      * 仓库id
      */
     @ApiModelProperty(name = "仓库id", notes = "")
     private String repositoryId;
     /**
      * 路径
      */
     @ApiModelProperty(name = "路径", notes = "")
     private String path;
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