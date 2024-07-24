package com.veadan.folib.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
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
import javax.persistence.Table;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
* 权限表;
* @author : Fengmaogen
* @date : 2024-7-17
*/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "权限表",description = "")
public class RoleResourceRefDTO implements Serializable,Cloneable {

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
    private List<String> storageProvileges;
    /**
     * 仓库权限
     */
    @ApiModelProperty(name = "仓库权限", notes = "")
    private String repositoryPrivilege;
    private List<String> repositoryPrivileges;
    /**
     * 路径权限
     */
    @ApiModelProperty(name = "路径权限", notes = "")
    private String pathPrivilege;
    private List<String> pathPrivileges;

}