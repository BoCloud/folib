package com.veadan.folib.dto;

import com.veadan.folib.entity.*;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
  * @Description: 用户权限同步请求参数
  * @auther: fengmg
  * @CreateDate: 2024/8/8 14:15
  * @Version: 1.0
  */
@Data
public class UserAuthReq {

    @ApiModelProperty("用户信息")
    private List<FolibUser> users;
    @ApiModelProperty("角色信息")
    private List<FolibRole> roles;
    @ApiModelProperty("用户组")
    private List<UserGroup> groups;
    @ApiModelProperty("资源")
    private List<Resource> resources;
    @ApiModelProperty("用户信息")
    private List<UserGroupRef> userGroups;
    @ApiModelProperty("用户信息")
    private List<RoleResourceRef> userRoles;
}
