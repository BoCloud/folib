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
package com.folib.users.service.impl;

import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.folib.dto.RoleResourceRefDTO;
import com.folib.entity.UserGroup;
import com.folib.entity.UserGroupRef;
import com.folib.mapper.UserGroupRefMapper;
import com.folib.users.service.UserGroupRefService;
import com.folib.users.service.UserGroupService;
import org.parboiled.common.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;


import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 用户组关联表;(user_group_ref)表服务实现类
 * @author veadan
 * @date : 2024-7-18
 */
@Service
@Transactional(rollbackFor=Exception.class)
public class UserGroupRefServiceImpl implements UserGroupRefService  {
    @Autowired
    private UserGroupRefMapper userGroupRefMapper;
    @Lazy
    @Autowired
    private UserGroupService userGroupService;
    
    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    @Override
    public UserGroupRef queryById(Long id){
        return userGroupRefMapper.queryById(id);
    }
    
    ///**
    // * 分页查询
    // *
    // * @param userGroupRef 筛选条件
    // * @param pageRequest 分页对象
    // * @return 查询结果
    // */
    //public IPage<UserGroupRef> paginQuery(UserGroupRef userGroupRef, PageRequest pageRequest){
    //    long total = userGroupRefMapper.count(userGroupRef);
    //    com.baomidou.mybatisplus.extension.plugins.pagination.Page<UserGroupRef> page = new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(pageRequest.getPageNumber(), pageRequest.getPageSize());
    //  return   userGroupRefMapper.queryAllByLimit(page, userGroupRef);
    //}
    
    /** 
     * 新增数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    @Override
    public UserGroupRef insert(UserGroupRef userGroupRef){
        updateGroupName(Collections.singletonList(userGroupRef));
        userGroupRefMapper.insert(userGroupRef);
        return userGroupRef;
    }
    
    /** 
     * 更新数据
     *
     * @param userGroupRef 实例对象
     * @return 实例对象
     */
    @Override
    public UserGroupRef update(UserGroupRef userGroupRef){
        updateGroupName(Collections.singletonList(userGroupRef));
        userGroupRefMapper.update(userGroupRef);
        return queryById(userGroupRef.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(Long id){
        int total = userGroupRefMapper.deleteById(id);
        return total > 0;
    }

    /**
     * 批量保存用户组关系
     * @param entities
     * @return
     */
    @Override
    public int saveBath(List<UserGroupRef> entities) {
        updateGroupName(entities);
        return userGroupRefMapper.insertBatch(entities);
    }

    private void updateGroupName(List<UserGroupRef> entities) {
        List<Long> groupIds = entities.stream().filter(userGroupRef -> userGroupRef.getUserGroupName() == null).map(UserGroupRef::getUserGroupId).collect(Collectors.toList());
        if (!groupIds.isEmpty()) {
            List<UserGroup> userGroups = userGroupService.queryByIds(groupIds);
            Map<Long, String> nameMap = userGroups.stream().collect(Collectors.toMap(UserGroup::getId, UserGroup::getGroupName));
            entities.forEach(userGroupRef -> {
                    if (StringUtils.isEmpty(userGroupRef.getUserGroupName())) {
                        userGroupRef.setUserGroupName(nameMap.get(userGroupRef.getUserGroupId()));
                    }
            });
        }
    }

    @Override
    public List<RoleResourceRefDTO> queryPrivilegeByGroup(Long groupId, String refType, List<String> roleIds) {
        return userGroupRefMapper.queryPrivilegeByGroup(groupId, refType, roleIds);
    }

    @Override
    public void deleteByUserGroupId(Long id) {
        userGroupRefMapper.delete(Wrappers.<UserGroupRef>lambdaQuery().eq(UserGroupRef::getUserGroupId, id));
    }

    @Override
    public void deleteByUserId(String userId) {
        userGroupRefMapper.delete(Wrappers.<UserGroupRef>lambdaQuery().eq(UserGroupRef::getUserId, userId));
    }

    @Override
    public void batchUpdate(List<UserGroupRef> userGroups) {
        List<Long> groupIds = userGroups.stream().map(UserGroupRef::getUserGroupId).collect(Collectors.toList());
        List<UserGroupRef> queryUserGroupRefs = userGroupRefMapper.queryByGroupIds(groupIds);
        if (!queryUserGroupRefs.isEmpty()) {
            userGroups = userGroups.stream().filter(userGroupRef -> queryUserGroupRefs.stream().noneMatch(queryUserGroupRef -> queryUserGroupRef.getUserGroupId().equals(userGroupRef.getUserGroupId()) && queryUserGroupRef.getUserId().equals(userGroupRef.getUserId()))).collect(Collectors.toList());
        }
        if (!userGroups.isEmpty()) {
            userGroupRefMapper.insertBatch(userGroups);
        }
    }

    @Override
    public List<UserGroupRef> queryByGroupIds(List<Long> groupIds) {
        if (CollectionUtils.isEmpty(groupIds)) {
            return Collections.emptyList();
        }
        return userGroupRefMapper.queryByGroupIds(groupIds);
    }

    @Override
    public List<UserGroupRef> queryByUserId(String userId) {
        return userGroupRefMapper.selectList(Wrappers.<UserGroupRef>lambdaQuery().eq(UserGroupRef::getUserId, userId));
    }

    @Override
    public void deleteByIds(List<Long> refIds) {
        userGroupRefMapper.delete(Wrappers.<UserGroupRef>lambdaQuery().in(UserGroupRef::getId, refIds));
    }


}