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

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.folib.dto.RoleResourceRefDTO;
import com.folib.entity.UserGroupRef;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

 /**
 * 用户组关联表;(user_group_ref)表数据库访问层
 * @author veadan
 * @date : 2024-7-17
 */
@Mapper
public interface UserGroupRefMapper extends BaseMapper<UserGroupRef> {
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    UserGroupRef queryById(Long id);
    /** 
     * 分页查询指定行数据
     *
     * @param userGroupRef 查询条件
     * @param pageable 分页对象
     * @return 对象列表
     */
    IPage<UserGroupRef> queryAllByLimit(Page<UserGroupRef> pageable, @Param("entity") UserGroupRef userGroupRef);
    /** 
     * 统计总行数
     *
     * @param userGroupRef 查询条件
     * @return 总行数
     */
    long count(UserGroupRef userGroupRef);
    /** 
     * 新增数据
     *
     * @param userGroupRef 实例对象
     * @return 影响行数
     */
    int insert(UserGroupRef userGroupRef);
    /** 
     * 批量新增数据
     *
     * @param entities List<UserGroupRef> 实例对象列表
     * @return 影响行数
     */
    int insertBatch(@Param("entities") List<UserGroupRef> entities);
    /** 
     * 批量新增或按主键更新数据
     *
     * @param entities List<UserGroupRef> 实例对象列表
     * @return 影响行数
     */
    int insertOrUpdateBatch(@Param("entities") List<UserGroupRef> entities);
    /** 
     * 更新数据
     *
     * @param userGroupRef 实例对象
     * @return 影响行数
     */
    int update(UserGroupRef userGroupRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 影响行数
     */
    int deleteById(Long id);

     List<RoleResourceRefDTO> queryPrivilegeByGroup(@Param("entityId")Long groupId,@Param("refType") String refType, @Param("roleIds") List<String> roleIds);

     List<UserGroupRef> queryByGroupIds(@Param("groupIds") List<Long> groupIds);
 }