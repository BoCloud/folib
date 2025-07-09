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
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.folib.components.IdGenerateUtils;
import com.folib.constant.GlobalConstants;
import com.folib.dto.UserGroupDTO;
import com.folib.dto.UserGroupListDTO;
import com.folib.entity.UserGroup;
import com.folib.entity.UserGroupRef;
import com.folib.mapper.UserGroupMapper;
import com.folib.users.service.RoleResourceRefService;
import com.folib.users.service.UserGroupRefService;
import com.folib.users.service.UserGroupService;
import com.folib.utils.UserManageUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 用户组;(user_group)表服务实现类
 * @author veadan
 * @date : 2024-7-17
 */
@Service
@Transactional(rollbackFor=Exception.class)
public class UserGroupServiceImpl implements UserGroupService {
    @Autowired
    private UserGroupMapper userGroupMapper;
    @Inject
    @Lazy
    private IdGenerateUtils idGenerateUtils;
    @Lazy
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Lazy
    @Autowired
    private RoleResourceRefService roleResourceRefService;

    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    @Override
    public UserGroup queryById(Long id){
        return userGroupMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param userGroup 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    @Override
    public PageInfo<UserGroupListDTO> paginQuery(UserGroup userGroup, PageRequest pageRequest){
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<UserGroupListDTO> userGroupListDTOS = userGroupMapper.queryAllByLimit(userGroup);
        return new PageInfo<>(userGroupListDTOS);
    }

    @Override
    public PageInfo<UserGroupListDTO> pageQueryAndUserNumber(UserGroup userGroup, PageRequest pageRequest) {
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<UserGroupListDTO> userGroupListDTOS = userGroupMapper.queryAllByUser(userGroup);
        return new PageInfo<>(userGroupListDTOS);
    }

    /** 
     * 新增数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    @Override
    public UserGroup save(UserGroup userGroup){
        String groupName = userGroup.getGroupName();
        List<UserGroup> userGroups = queryByGroupNames(Collections.singletonList(groupName));
        if (CollectionUtils.isNotEmpty(userGroups) && userGroups.get(0).getGroupName().equals(groupName)) {
            throw new RuntimeException("UserGroupName is already");
        }
        userGroup.setCreateBy(UserManageUtils.getUsername());
        userGroup.setId(idGenerateUtils.generateId("userGroupId"));
        userGroupMapper.insert(userGroup);
        return userGroup;
    }
    
    /** 
     * 更新数据
     *
     * @param userGroup 实例对象
     * @return 实例对象
     */
    @Override
    public UserGroup update(UserGroup userGroup){
        String groupName = userGroup.getGroupName();
        List<UserGroup> userGroups = queryByGroupNames(Collections.singletonList(groupName));
        if (CollectionUtils.isNotEmpty(userGroups)
                && userGroups.get(0).getGroupName().equals(groupName)
                && !userGroups.get(0).getId().equals(userGroup.getId())) {
            throw new RuntimeException("UserGroupName is already");
        }
        userGroup.setUpdateBy(UserManageUtils.getUsername());
        userGroupMapper.update(userGroup);
        //批量更新用户组关联用户表中的用户组名称冗余字段
        batchUpdateRefGroupName(Collections.singletonList(userGroup.getId()));
        return queryById(userGroup.getId());
    }

    /**
     * 批量更新用户组名称
     * @param groupIds 用户组id
     */
    private void batchUpdateRefGroupName(List<Long> groupIds) {
        List<UserGroup> userGroups = queryByIds(groupIds);
        if (CollectionUtils.isNotEmpty(userGroups)) {
            Map<Long, String> groupNameMap = userGroups.stream().collect(Collectors.toMap(UserGroup::getId, UserGroup::getGroupName));
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                List<UserGroupRef> updateRefs = userGroupRefs.stream().filter(userGroupRef -> !Objects.equals(groupNameMap.get(userGroupRef.getUserGroupId()), userGroupRef.getUserGroupName())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(updateRefs)) {
                    userGroupRefService.batchUpdate(updateRefs);
                }
            }
        }
    }

    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(Long id){
        int update = userGroupMapper.deleteById(id);
        //删除用户组关联用户
        userGroupRefService.deleteByUserGroupId(id);
        //删除角色关联用户组
        roleResourceRefService.deleteByentityId(String.valueOf(id), GlobalConstants.ROLE_TYPE_USER_GROUP);
        return update > 0;
    }

    @Override
    public List<UserGroup> queryUserGroupList(UserGroup userGroup) {
       return userGroupMapper.selectList(Wrappers.<UserGroup>lambdaQuery()
                .eq(UserGroup::getJoinGroup, userGroup.getJoinGroup())
                .eq(UserGroup::getDeleted, userGroup.getDeleted())
                .eq(userGroup.getGroupName()!=null,UserGroup::getGroupName, userGroup.getGroupName())
        );
    }

    @Override
    public List<UserGroup> findAll() {
      return userGroupMapper.selectList(Wrappers.<UserGroup>lambdaQuery().eq(UserGroup::getDeleted, GlobalConstants.NOT_DELETED));
    }

    @Override
    public UserGroupDTO queryGroupDetailById(Long groupId) {
        return userGroupMapper.queryGroupDetailById(groupId);
    }

    @Override
    public void saveOrUpdateBatch(List<UserGroup> groups) {
        userGroupMapper.insertOrUpdateBatch(groups);
        batchUpdateRefGroupName(groups.stream().map(UserGroup::getId).collect(Collectors.toList()));
    }

    @Override
    public List<UserGroup> queryByIds(List<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return Collections.emptyList();
        }
        return userGroupMapper.selectList(Wrappers.<UserGroup>lambdaQuery().in(UserGroup::getId, ids));
    }

    @Override
    public List<UserGroup> queryByGroupNames(List<String> groupNames) {
        if (CollectionUtils.isEmpty(groupNames)) {
            return Collections.emptyList();
        }
        return userGroupMapper.selectList(Wrappers.<UserGroup>lambdaQuery().in(UserGroup::getGroupName, groupNames));
    }
}