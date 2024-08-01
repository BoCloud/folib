package com.veadan.folib.controllers.users.support;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.veadan.folib.entity.FolibRole;
import io.swagger.annotations.ApiModelProperty;

import java.io.Serializable;

/**
 * @author veadan
 * @author Veadan
 * @JsonInclude used because com.veadan.folib.users.domain.User is annotated with it
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class UserRoleOutput extends FolibRole
        implements Serializable
{
    /**
     * 存储空间权限
     */
    @ApiModelProperty(name = "存储空间权限", notes = "")
    private String storagePrivilege;
    /**
     * 仓库权限
     */
    @ApiModelProperty(name = "仓库权限", notes = "")
    private String repositoryPrivilege;
    /**
     * 路径权限
     */
    @ApiModelProperty(name = "路径权限", notes = "")
    private String pathPrivilege;

}
