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
package com.folib.mapper;
import java.util.List;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.folib.dto.PermissionsDTO;
import com.folib.dto.RoleDTO;
import com.folib.dto.UserRoleDTO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import com.folib.entity.RoleResourceRef;
import org.springframework.stereotype.Component;

/**
 * 权限表;(role_resource_ref)表数据库访问层
 * @author veadan
 * @date : 2024-7-17
 */
@Component
@Mapper
public interface RoleResourceRefMapper extends BaseMapper<RoleResourceRef> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    RoleResourceRef queryById(Long id);
    ///**
    // * 分页查询指定行数据
    // *
    // * @param roleResourceRef 查询条件
    // * @param pageable 分页对象
    // * @return 对象列表
    // */
    //List<RoleResourceRef> queryAllByLimit(RoleResourceRef roleResourceRef, @Param("pageable") Pageable pageable);
    /** 
     * 统计总行数
     *
     * @param roleResourceRef 查询条件
     * @return 总行数
     */
    long count(RoleResourceRef roleResourceRef);
    /** 
     * 新增数据
     *
     * @param roleResourceRef 实例对象
     * @return 影响行数
     */
    @Override
    int insert(RoleResourceRef roleResourceRef);
    /** 
     * 批量新增数据
     *
     * @param entities List<RoleResourceRef> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<RoleResourceRef> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<RoleResourceRef> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<RoleResourceRef> entities);
    /** 
     * 更新数据
     *
     * @param roleResourceRef 实例对象
     * @return 影响行数
     */
    int update(RoleResourceRef roleResourceRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);

     List<RoleResourceRef> queryAllByRoleId(@Param("roleIds") List<String> roleIds);
     /**
      * 通过主键批量删除数据
      *
      * @param refIds 主键
      * @return 影响行数
      */
     int deleteByRefIds(@Param("refIds") List<Long> refIds);

     List<UserRoleDTO> queryPrivileges(@Param("roleIds") List<String> roleIds);

     RoleDTO getUserByRoleId(@Param("roleId") String roleId);

     /**
      *
      * @param roleId
      * @param username
      * @param storageId
      * @param repositoryId
      * @param resourceEmpty true值则查询资源为空的权限（admin角色直接和用户关联，没有关联资源）
      * @return
      */
     List<PermissionsDTO> queryPermissions(@Param("roleId") String roleId, @Param("username") String username,
                                           @Param("storageId") String storageId, @Param("repositoryId")String repositoryId,
                                           @Param("storageIds") List<String> storageIds,
                                           @Param("resourceIds") List<String> resourceIds, @Param("resourceEmpty") boolean resourceEmpty);

     List<RoleResourceRef> queryRoleByUserId(@Param("userId")String userId,@Param("roleIds") List<String> roleIds);

     /**
      * 通过角色id查询api权限
      * @param roleIds
      * @return
      */
     List<RoleResourceRef> queryApiAuthorities(@Param("roleIds") List<String> roleIds);

     List<RoleResourceRef> queryByRoleIds(@Param("roleIds") List<String> roleIds);

     int deleteByRoleId(@Param("roleId") String roleId);

     List<RoleResourceRef> queryResourcesByRoleIds(@Param("roleIds") List<String> roleIds);
 }