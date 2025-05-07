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
import java.util.HashSet;
import java.util.Set;

/**
 * 用户信息;
 * @author : fengmaogen
 * @date : 2024-7-9
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@TableName("folib_user")
@ApiModel(value = "用户信息",description = "")
public class FolibUser implements Serializable,Cloneable{
     private static final long serialVersionUID = 1L;
    /** 主键 */

    @ApiModelProperty(name = "主键",notes = "")
    private String id ;
    /** 用户名 */
    @ApiModelProperty(name = "用户名",notes = "")
    private String username ;
    private transient String matchUsername;
    /** 密码 */
    @ApiModelProperty(name = "密码",notes = "")
    private String password ;
    /** 原始密码 */
    @ApiModelProperty(name = "原始密码",notes = "")
    private String originalPassword ;
    /** 头像 */
    @ApiModelProperty(name = "头像",notes = "")
    private String avatar ;
    /** 邮件 */
    @ApiModelProperty(name = "邮件",notes = "")
    private String email ;
    private transient String matchEmail;
    /** 用户类型 */
    @ApiModelProperty(name = "用户类型",notes = "")
    private String userType ;
    /** 是否启用 */
    @ApiModelProperty(name = "是否启用",notes = "")
    private String enabled ;
    /** 来源 */
    @ApiModelProperty(name = "来源",notes = "")
    private String sourceId ;
    /** 是否删除 */
    @ApiModelProperty(name = "是否删除",notes = "")
    private String deleted = "0" ;
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

    /**
     * 昵称
     */
    private String nickname;

    private transient String refType;
    private transient Set<String> roles = new HashSet<>();
}