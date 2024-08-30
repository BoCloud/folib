package com.veadan.folib.controllers.users.support;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;

import java.io.Serializable;
import java.util.List;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.veadan.folib.users.domain.User is annotated with it
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserGroupOutput
        implements Serializable
{
    /**
     * 主键
     */
    @ApiModelProperty(name = "主键", notes = "")
    private Long id;
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

    private List<String> userIds;
    private List<String> roleIds;

}
