package com.folib.dto;

import com.folib.entity.*;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
  * @Description: 用户权限同步请求参数
  * @auther: fengmg
  * @CreateDate: 2024/8/8 14:15
  * @Version: 1.0
  */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserAuthReq {

    @ApiModelProperty("用户信息")
    protected List<FolibUser> users;
    @ApiModelProperty("角色信息")
    protected List<FolibRole> roles;
    @ApiModelProperty("用户组")
    protected List<UserGroup> groups;
    @ApiModelProperty("资源")
    protected List<Resource> resources;
    @ApiModelProperty("用户组信息")
    protected List<UserGroupRef> userGroups;
    @ApiModelProperty("权限信息")
    protected List<RoleResourceRef> userRoles;

    //TODO 存储空间、仓库

    protected boolean nextPage;
}
