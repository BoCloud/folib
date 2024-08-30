package com.veadan.folib.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Id;
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;

 /**
 * 角色信息;
 * @author : Fengmaogen
 * @date : 2024-7-17
 */

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "角色信息",description = "")
@Table(name="folib_role")
public class FolibRole implements Serializable,Cloneable {
     /**
      * 主键
      */
     @Id
     @ApiModelProperty(name = "主键", notes = "")
     private String id;
     /**
      * 中文名称
      */
     @ApiModelProperty(name = "中文名称", notes = "")
     private String cnName;
     /**
      * 英文名称
      */
     @ApiModelProperty(name = "英文名称", notes = "")
     private String enName;
     /**
      * 角色描述
      */
     @ApiModelProperty(name = "角色描述", notes = "")
     private String description;
     /**
      * 是否删除
      */
     @ApiModelProperty(name = "是否删除", notes = "")
     private String deleted;
     /** 是否默认 */
     @ApiModelProperty(name = "是否默认", notes = "")
     private String isDefault;
     /**
      * 创建人
      */
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

      private transient String storageId;
      private transient String repositoryId;
      private transient String path;

 }