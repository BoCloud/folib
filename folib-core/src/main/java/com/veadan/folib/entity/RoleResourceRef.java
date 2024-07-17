package com.veadan.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

 /**
 * 权限表;
 * @author : http://www.chiner.pro
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "权限表",description = "")
@Table(name="role_resource_ref")
public class RoleResourceRef implements Serializable,Cloneable {
     /**
      * 主键
      */
     @Id
     @GeneratedValue
     @ApiModelProperty(name = "主键", notes = "")
     private String id;
     /**
      * 角色id
      */
     @ApiModelProperty(name = "角色id", notes = "")
     private Integer roleId;
     /**
      * 对象id
      */
     @ApiModelProperty(name = "对象id", notes = "")
     private String entityId;
     /**
      * 关联类型[用户、用户组];[1-用户id、2-用户组id]
      */
     @ApiModelProperty(name = "关联类型[用户、用户组]", notes = "[1-用户id、2-用户组id]")
     private String refType;
     /**
      * 资源id;[1-api、2-存储空间、3-仓库、4-路径]
      */
     @ApiModelProperty(name = "资源id", notes = "[1-api、2-存储空间、3-仓库、4-路径]")
     private String resourceId;
     /**
      * 存储空间权限
      */
     @ApiModelProperty(name = "存储空间权限", notes = "")
     private String storageProvilege;
     /**
      * 仓库权限
      */
     @ApiModelProperty(name = "仓库权限", notes = "")
     private String repositoryPrivilege;
     /**
      * 路径权限
      */
     @ApiModelProperty(name = "路径权限", notes = "")
     private String pathPrivilege;
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