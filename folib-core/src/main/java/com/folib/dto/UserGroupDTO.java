package com.folib.dto;

import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.fasterxml.jackson.annotation.JsonIgnore;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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
public class UserGroupDTO implements Serializable,Cloneable {
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

     @JsonIgnore
    private String users;

    public List<String> getUserIds() {
        if (StringUtils.isEmpty(users)) {
            return Collections.emptyList();
        }
        return Arrays.asList(users.split(","));
    }

    public List<String> getRoleIds() {
        if (StringUtils.isEmpty(roles)){
           return Collections.emptyList();
        }
        return Arrays.asList(roles.split(","));
    }

    private List<String> userIds;
    @JsonIgnore
    private String roles;
    private List<String> roleIds;

}