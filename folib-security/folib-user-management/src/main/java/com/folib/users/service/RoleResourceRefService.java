/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.users.service;

import com.folib.dto.PermissionsDTO;
import com.folib.dto.RoleDTO;
import com.folib.dto.UserRoleDTO;
import com.folib.entity.FolibUser;
import com.folib.users.dto.UserPermissionDTO;
import com.folib.entity.RoleResourceRef;

import java.util.List;
import java.util.Set;

/**
 * 权限表;(role_resource_ref)表服务接口
 * @author veadan
 * @date : 2024-7-18
 */
public interface RoleResourceRefService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    RoleResourceRef queryById(Long id);
    ///**
    // * 分页查询
    // *
    // * @param roleResourceRef 筛选条件
    // * @param pageRequest 分页对象
    // * @return 查询结果
    // */
    //Page<RoleResourceRef> paginQuery(RoleResourceRef roleResourceRef, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    RoleResourceRef insert(RoleResourceRef roleResourceRef);
    /** 
     * 更新数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    RoleResourceRef update(RoleResourceRef roleResourceRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(Long id);

    /**
     * 批量保存用户权限
     * @param roleResourceRefs
     * @return 保存数量
     */
     int saveBath(List<RoleResourceRef> roleResourceRefs);
    /**
     * 列表查询
     *
     * @param roleResourceRef 筛选条件
     * @return 查询结果
     */
    List<RoleResourceRef> queryRefs(RoleResourceRef roleResourceRef);

    /**
     * 根据角色Id查询权限列表
     * @param roleIds
     * @return
     */
    List<RoleResourceRef> queryRefsByRoleIds(List<String> roleIds);

    void removeByIds(List<Long> removeRefIds);

    List<UserRoleDTO> getRolesByUserName(String userName);

    RoleDTO getUserByRoleId(String roleId);

    List<PermissionsDTO> queryPermissions(String roleId,String username, String storageId, String repositoryId);
    List<PermissionsDTO> queryPermissions(String roleId, String username, String storageId, String repositoryId, boolean resourceEmpty);

    List<PermissionsDTO> queryPermissionsByResourceIds(List<String> resourceIds);
    boolean deleteByRoleId(String roleId);

    List<RoleResourceRef> queryRoleByUserId(String uuid, List<String> roles);

    List<PermissionsDTO> queryPermissionsByStorageIds(List<String> storageIds);

    void savePermissions(RoleDTO roleForm, String roleId, String username);

    List<RoleResourceRef> queryApiAuthorities(List<String> roleIds);

    void batchUpdate(List<RoleResourceRef> userRoles);

    List<RoleResourceRef> queryByRoleIds(List<String> roleIds);

    void deleteByentityId(String entityId, String refType);

    void deleteByIds(List<Long> removeIds);

    List<RoleResourceRef> queryPermissionsByRoleIds(List<String> roleIds);
    List<RoleResourceRef> queryByUserIds(List<String> userIds);

    void updateUserPermission(Set<UserPermissionDTO> userPermissions);
    /**
     *
     * 方法描述: 根据角色id查询关联的用户列表
     *
     * @param: 角色id
     * @return: 用户详情集合
     */
    List<FolibUser> queryUserByRoleIds(String roleId);

    void deleteAllByRoleId(String roleId);
    void deleteAnonymousRole(String roleId);

    /**
     *  更新存储空间关联用户
     * @param userPermission 用户权限
     */
    void updateStorageUser(UserPermissionDTO userPermission);

    void deleteByResourceIds(List<String> resourceId);

    List<RoleResourceRef> queryByResourceIds(List<String> resourceIds);
    List<RoleResourceRef> queryByIds(List<Long> ids);

    List<RoleResourceRef> queryResourcesByRoleIds(List<String> roleIds);

     void deleteAllByRoleIdAndEntityNotNull(String roleId);
}