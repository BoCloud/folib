package com.veadan.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Column;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import java.io.Serializable;
import java.util.Date;

 /**
 * 用户组;
 * @author : Fengmaogen
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "用户组",description = "")
public class UserGroup implements Serializable,Cloneable {
     /**
      * 主键
      */
     @Id
     @ApiModelProperty(name = "主键", notes = "")
     private Long id;
     /**
      * 组名称
      */
     @ApiModelProperty(name = "组名称", notes = "")
     private String groupName;
     /**模糊匹配用户组名称**/
     private transient String matchGroupName;
     /**
      * 描述
      */
     @ApiModelProperty(name = "描述", notes = "")
     private String description;
     /**
      * 新建用户是否自动加入此用户组
      */
     @ApiModelProperty(name = "新建用户是否自动加入此用户组", notes = "")
     private String joinGroup;
     /**
      * 是否删除
      */
     @ApiModelProperty(name = "是否删除", notes = "")
     @Column(name = "deleted")
     private String deleted;
      /** 是否默认 */
      @ApiModelProperty(name = "是否默认", notes = "")
      @Column(name = "is_default")
      private String isDefault;
     /** 创建人 */
     @ApiModelProperty(name = "创建人",notes = "")
     private String createBy ;
     /** 创建时间 */
     @ApiModelProperty(name = "创建时间",notes = "")
     @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
     private Date createTime ;
     /** 更新人 */
     @ApiModelProperty(name = "更新人",notes = "")
     private String updateBy ;
     /** 更新时间 */
     @ApiModelProperty(name = "更新时间",notes = "")
     @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
     private Date updateTime ;

 }