package com.folib.entity;

import com.baomidou.mybatisplus.annotation.TableField;
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
 * 用户组关联表;
 * @author veadan
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "用户组关联表",description = "")
@TableName("user_group_ref")
public class UserGroupRef implements Serializable,Cloneable {
     /**
      * 主键
      */
     @TableId
     @ApiModelProperty(name = "主键", notes = "")
     private Long id;
     /**
      * 用户组id
      */
     @ApiModelProperty(name = "用户组id", notes = "")
     private Long userGroupId;
     @ApiModelProperty(name = "组名称", notes = "")
     private String userGroupName;
     /**
      * 角色id
      */
     @ApiModelProperty(name = "用户id", notes = "")
     private String userId;
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