package com.veadan.folib.forms.users;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

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
@JsonIgnoreProperties(ignoreUnknown = true)
public class UserGroupForm implements Serializable,Cloneable {
    /**
     * 主键
     */
    @NotEmpty(groups = {ExistingUserGroup.class }, message = "userGroup id is required!")
    @ApiModelProperty(name = "主键", notes = "")
    private Long id;
    /**
     * 组名称
     */
    @NotEmpty(groups = { NewUserGroup.class }, message = "userGroup name is required!")
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
     /** 用户id */
     private List<String> userIds;

    public interface NewUserGroup
            extends Serializable
    {
        // validation group marker interface for new users.
    }

    public interface ExistingUserGroup
            extends Serializable
    {
        // validation group marker interface for existing users.
    }
}