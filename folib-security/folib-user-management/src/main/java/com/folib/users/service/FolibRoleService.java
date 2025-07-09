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

import com.github.pagehelper.PageInfo;
import com.folib.dto.FolibRoleDTO;
import com.folib.dto.RoleDTO;
import com.folib.entity.FolibRole;
import com.folib.storage.repository.RepositoryPermissionDto;
import org.springframework.data.domain.PageRequest;

import java.util.List;
import java.util.Set;

/**
 * 角色信息;(folib_role)表服务接口
 * @author veadan
 * @date : 2024-7-17
 */
public interface FolibRoleService{
    //通过用户查询用户关联的权限

    //通过角色id查询查询权限
     FolibRole queryByRoleId(List<String> roleIds);
     /**
      * 同步配置权限
      */
     void syncYamlAuthorizationConfig();
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    FolibRole queryById(String id);
    /** 
     * 分页查询
     *
     * @param folibRole 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    PageInfo<FolibRoleDTO> paginQuery(FolibRole folibRole, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    FolibRole insert(FolibRole folibRole);
    /** 
     * 更新数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    FolibRole update(FolibRole folibRole);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(String id);

    void save(RoleDTO roleDTO, String username);



    void updateRoleInfo(RoleDTO roleDTO, String roleId, String username);

    void deleteUserRoleCache(List<String> userIds);

    List<FolibRole> queryRoles(FolibRole build);

    /**
     * 获取角色详情
     * @param roleId 角色id
     * @param folibRole 角色信息
     * @return 权限信息
     */
    RoleDTO getRoleDetail(String roleId, FolibRole folibRole);

    void deleteRole(String roleId);

    void saveOrUpdateBatch(List<FolibRole> roles);

    List<FolibRole> queryByIds(Set<String> roles);

    void updateRepostoryPermission(String storageId, String repositoryId, RepositoryPermissionDto repositoryPermissionDto);
}