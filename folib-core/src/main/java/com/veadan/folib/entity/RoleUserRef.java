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
 * 角色用户关联表;
 * @author : http://www.chiner.pro
 * @date : 2024-7-17
 */
 @Data
 @Builder
 @AllArgsConstructor
 @NoArgsConstructor
 @Accessors(chain = true)
@ApiModel(value = "角色用户关联表",description = "")
@Table(name="role_user_ref")
public class RoleUserRef implements Serializable,Cloneable {
     /**
      * 主键
      */
     @Id
     @GeneratedValue
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