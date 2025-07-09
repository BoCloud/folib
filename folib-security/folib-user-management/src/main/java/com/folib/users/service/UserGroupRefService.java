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

import com.folib.dto.RoleResourceRefDTO;
import com.folib.entity.UserGroupRef;

import java.util.List;

/**
 * 用户组关联表;(user_group_ref)表服务接口
 * @author veadan
 * @date : 2024-7-18
 */
public interface UserGroupRefService{
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    UserGroupRef queryById(Long id);
    ///**
    // * 分页查询
    // *
    // * @param userGroupRef 筛选条件
    // * @param pageRequest 分页对象
    // * @return 查询结果
    // */
    //IPage<UserGroupRef> paginQuery(UserGroupRef userGroupRef, PageRequest pageRequest);
    /** 
     * 新增数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    UserGroupRef insert(UserGroupRef userGroupRef);
    /** 
     * 更新数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    UserGroupRef update(UserGroupRef userGroupRef);
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    boolean deleteById(Long id);
    /**
     * 批量保存用户组关系
     * @param entities
     * @return
     */
    int saveBath(List<UserGroupRef> entities);

    List<RoleResourceRefDTO> queryPrivilegeByGroup(Long groupId, String refType, List<String> roleIds);

    void deleteByUserGroupId(Long id);
    void deleteByUserId(String userId);

    void batchUpdate(List<UserGroupRef> userGroups);

    List<UserGroupRef> queryByGroupIds(List<Long> groupIds);
    List<UserGroupRef> queryByUserId(String userId);

    void deleteByIds(List<Long> refIds);
}