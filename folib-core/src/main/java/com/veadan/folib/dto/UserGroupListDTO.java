package com.veadan.folib.dto;

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
* 用户组;
* @author veadan
* @date : 2024-7-17
*/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "用户组",description = "")
public class UserGroupListDTO implements Serializable,Cloneable {
    /**
     * 主键
     */
    @ApiModelProperty(name = "主键", notes = "")
    private String id;
    /**
     * 组名称
     */
    @ApiModelProperty(name = "组名称", notes = "")
    private String groupName;
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
    private String deleted;
     /** 是否默认 */
     @ApiModelProperty(name = "是否默认", notes = "")
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

    private int userCount;

    private String roles;
}