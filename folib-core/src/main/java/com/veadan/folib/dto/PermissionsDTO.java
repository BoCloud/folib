package com.veadan.folib.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.persistence.Id;
import java.io.Serializable;

/**
* 用户组关联表;
* @author : Fengmaogen
* @date : 2024-7-17
*/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
public class PermissionsDTO implements Serializable,Cloneable {
    /**
     * 主键
     */
    @Id
    @ApiModelProperty(name = "主键", notes = "")
    private Long id;
    /**
     * 角色id
     */
    @ApiModelProperty(name = "角色id", notes = "")
    private String roleId;
    /**
     * 对象id
     */
    @ApiModelProperty(name = "对象id", notes = "")
    private String entityId;
    /**
     * 关联类型[用户、用户组];[1-用户id、2-用户组id]
     */
    @ApiModelProperty(name = "关联类型[用户、用户组]", notes = "[1-用户id、2-用户组id]")
    private String refType;
    /**
     * 资源id
     */
    @ApiModelProperty(name = "资源id", notes = "")
    private Long resourceId;
    /**
     * 资源类型;[1-api、2-存储空间、3-仓库、4-路径]
     */
    @ApiModelProperty(name = "资源类型", notes = "")
    private String resourceType;
    /**
     * 存储空间权限
     */
    @ApiModelProperty(name = "存储空间权限", notes = "")
    private String storageProvilege;
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

    /**
     * api权限
     */
    @ApiModelProperty(name = "api权限", notes = "")
    private String apiAuthoritie;
    /**
     * 存储空间id
     */
    @ApiModelProperty(name = "存储空间id", notes = "")
    private String storageId;
    /**
     * 仓库id
     */
    @ApiModelProperty(name = "仓库id", notes = "")
    private String repositoryId;
    /**
     * 路径
     */
    @ApiModelProperty(name = "路径", notes = "")
    private String path;

    private String description;
}