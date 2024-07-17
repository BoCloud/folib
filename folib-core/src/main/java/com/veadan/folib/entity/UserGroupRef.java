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
 * 用户组关联表;
 * @author : http://www.chiner.pro
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "用户组关联表",description = "")
@Table(name="user_group_ref")
public class UserGroupRef implements Serializable,Cloneable {
     /**
      * 主键
      */
     @Id
     @GeneratedValue
     @ApiModelProperty(name = "主键", notes = "")
     private Long id;
     /**
      * 用户组id
      */
     @ApiModelProperty(name = "用户组id", notes = "")
     private Long userGroupId;
     /**
      * 角色id
      */
     @ApiModelProperty(name = "角色id", notes = "")
     private Long userId;
     /**
      * 创建人
      */
     @ApiModelProperty(name = "创建人", notes = "")
     private String createBy;
     /**
      * 创建时间
      */
     @ApiModelProperty(name = "创建时间",notes = "")
     @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
     private Date createTime ;

 }