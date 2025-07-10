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
import com.folib.dto.RepositoryPrivilegeDTO;
import com.folib.dto.UserDTO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import com.folib.entity.FolibUser;

 /**
 * 用户信息;(folib_user)表数据库访问层
 * @author veadan
 * @date : 2024-7-9
 */
@Mapper
public interface FolibUserMapper extends BaseMapper<FolibUser> {
     /**
      * 查询总数
      *
      * @param folibUser 查询条件
      * @return 总数
      */
     long countUserRole(@Param("user") FolibUser folibUser);
     /**
      * 分页查询指定行数据
      *
      * @param folibUser 查询条件
      * @return 对象列表
      */
     List<UserDTO> queryAllUserRoleByLimit(@Param("user") FolibUser folibUser);
    /**
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    FolibUser queryById(String id);
    /** 
     * 分页查询指定行数据
     *
     * @param folibUser 查询条件
     * @return 对象列表
     */
    List<FolibUser> queryAllByLimit(@Param("user") FolibUser folibUser);
    /** 
     * 统计总行数
     *
     * @param folibUser 查询条件
     * @return 总行数
     */
    long count(FolibUser folibUser);
    /** 
     * 新增数据
     *
     * @param folibUser 实例对象
     * @return 影响行数
     */
    int insert(FolibUser folibUser);
    /** 
     * 批量新增数据
     *
     * @param entities List<FolibUser> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<FolibUser> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<FolibUser> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<FolibUser> entities);
    /** 
     * 更新数据
     *
     * @param folibUser 实例对象
     * @return 影响行数
     */
    int update(FolibUser folibUser);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(String id);

     @Deprecated
     List<UserDTO> queryUserRoleByRepositoryAndPrivilege(@Param("repositoryPrivilegeDTOS") List<RepositoryPrivilegeDTO> repositoryPrivilegeDTOS);

     List<UserDTO> queryUsersNameResource(@Param("usernames") List<String> usernames, @Param("storageId") String storageId,
                                          @Param("repositoryId") String repositoryId, @Param("path") String path, @Param("refType") String refType);
 }