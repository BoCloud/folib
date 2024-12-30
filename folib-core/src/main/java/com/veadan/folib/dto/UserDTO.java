package com.veadan.folib.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.veadan.folib.users.domain.User is annotated with it
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserDTO
        implements Serializable
{

    private String username;
    private String password;

    private boolean enabled;

    private String email;

    private String avatar;
    private String sourceId;

    private Set<String> userGroups;
    private Set<String> userGroupIds;

    private Set<String> roles;
    private String storageId;
    private String repositoryId;
    private String path;

    @ApiModelProperty(name = "主键",notes = "")
    private String id ;
    @ApiModelProperty(name = "原始密码",notes = "")
    private String originalPassword ;
    @ApiModelProperty(name = "用户类型",notes = "")
    private String userType ;
    @ApiModelProperty(name = "是否删除",notes = "")
    private String deleted ;

    private Date updateTime;

    private String nickname;

    public void setRepositoryPrivilege(String repositoryPrivilege) {
        this.repositoryPrivilege =  new HashSet<>(Arrays.asList(repositoryPrivilege.split(",")));;
    }

    public void setStoragePrivilege(String storagePrivilege) {
        this.storagePrivilege = new HashSet<>(Arrays.asList(storagePrivilege.split(",")));
    }

    public void setPathPrivilege(String pathPrivilege) {
        this.pathPrivilege = new HashSet<>(Arrays.asList(pathPrivilege.split(",")));
    }

    private Set<String> storagePrivilege;
    private Set<String> repositoryPrivilege;
    private Set<String> pathPrivilege;
    private String securityTokenKey;

    private Set<String> authorities;
    public void setUserGroups(String userGroups) {
        if (userGroups != null) {
            this.userGroups = new HashSet<>(Arrays.asList(userGroups.split(",")));
        }
    }
    public void setRoles(String roles) {
        if (roles != null) {
            this.roles = new HashSet<>(Arrays.asList(roles.split(",")));
        }
    }

    public void setUserGroupIds(String userGroupIds) {
        if (userGroupIds != null) {
            this.userGroupIds = new HashSet<>(Arrays.asList(userGroupIds.split(",")));
        }
    }

}
