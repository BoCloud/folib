package com.veadan.folib.users.dto;

import com.veadan.folib.entity.*;
import com.veadan.folib.storage.Storage;
import com.veadan.folib.storage.StorageDto;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryDto;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;

import java.util.List;

/**
 * @Author: fengmg
 * @Date: 2024/8/10 08:57
 * @Description:
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserAuthDTO{

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

    protected boolean nextPage;

    /**存储空间*/
    private List<StorageDto> storages;
    /**仓库*/
    private List<RepositoryDto> repositorys;

    public List<UserGroupRef> getUserGroups() {
        if (CollectionUtils.isNotEmpty(userGroups)) {
            userGroups.forEach(userRole -> userRole.setId(null));
        }
        return userGroups;
    }

    public List<RoleResourceRef> getUserRoles() {
        if (CollectionUtils.isNotEmpty(userRoles)) {
            userRoles.forEach(userRole -> userRole.setId(null));
        }
        return userRoles;
    }
}
