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
package com.folib.services.impl;


import com.folib.entity.*;
import com.folib.event.repository.RepositoryEventListenerRegistry;
import com.folib.services.ConfigurationManagementService;
import com.folib.services.RepositoryManagementService;
import com.folib.services.StorageManagementService;
import com.folib.services.UserSyncService;
import com.folib.users.service.*;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.Storage;
import com.folib.storage.StorageDto;
import com.folib.storage.repository.Repository;
import com.folib.storage.repository.RepositoryDto;
import com.folib.users.dto.UserAuthDTO;
import com.folib.users.service.impl.RelationalDatabaseUserService.RelationalDatabase;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import javax.inject.Inject;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@Slf4j
@Component
@RelationalDatabase
@Transactional(rollbackFor = Exception.class)
public class UserSyncServiceImpl implements UserSyncService
{

    @Inject
    protected FolibUserService folibUserService;
    @Inject
    private UserGroupService userGroupService;
    @Inject
    private UserGroupRefService userGroupRefService;
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Inject
    private FolibRoleService folibRoleService;
    @Inject
    private ResourceService resourceService;
    @Inject
    private ConfigurationManagementService configurationManagementService;
    @Autowired
    private StorageManagementService storageManagementService;
    @Autowired
    private RepositoryEventListenerRegistry repositoryEventListenerRegistry;
    @Inject
    protected RepositoryPathResolver repositoryPathResolver;
    @Autowired
    private RepositoryManagementService repositoryManagementService;

    @Override
    @Transactional
    public void syncUserAuth(UserAuthDTO date) {

        //更新节点用户信息
        List<FolibUser> users = date.getUsers();
        if (CollectionUtils.isNotEmpty(users)) {
            folibUserService.saveOrUpdate(users);
        }
        //更新用户组信息
        List<UserGroup> groups = date.getGroups();
        List<UserGroupRef> userGroups = date.getUserGroups();
        if (CollectionUtils.isNotEmpty(groups) || CollectionUtils.isNotEmpty(userGroups)) {
            List<String> groupNames = groups.stream().map(UserGroup::getGroupName).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(userGroups)) {
                groupNames.addAll(userGroups.stream().map(UserGroupRef::getUserGroupName).collect(Collectors.toList()));
            }
            List<UserGroup> userGroupList = userGroupService.queryByGroupNames(groupNames);
            Map<String, Long> userGroupMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(userGroupList)) {
                userGroupMap = userGroupList.stream().collect(Collectors.toMap(UserGroup::getGroupName, UserGroup::getId, (existing, replacement) -> existing));
            }

            if (CollectionUtils.isNotEmpty(groups)) {
                Map<String, Long> finalUserGroupMap = userGroupMap;
                List<UserGroup> addGroups = new ArrayList<>();
                groups.forEach(userGroup -> {
                    Long groupId = finalUserGroupMap.get(userGroup.getGroupName());
                    if (groupId == null) {
                        addGroups.add(userGroup);
                    }
                });
                if (CollectionUtils.isNotEmpty(addGroups)) {
                    userGroupService.saveOrUpdateBatch(addGroups);
                }
            }
            //更新用户组关联信息
            if (CollectionUtils.isNotEmpty(userGroups)) {
                Map<String, Long> finalUserGroupMap1 = userGroupMap;
                userGroups.forEach(userGroupRef -> {
                        Long groupId = finalUserGroupMap1.get(userGroupRef.getUserGroupName());
                        if (groupId != null) {
                            userGroupRef.setUserGroupId(groupId);
                        }
                    });
                userGroupRefService.batchUpdate(userGroups);
            }
        }


        //更新角色信息
        List<FolibRole> roles = date.getRoles();
        if (CollectionUtils.isNotEmpty(roles)) {
            folibRoleService.saveOrUpdateBatch(roles);
        }
        //更新资源信息
        List<Resource> resources = date.getResources();
        if (CollectionUtils.isNotEmpty(resources)) {
            resourceService.saveOrUpdateBatch(resources);
        }
        //更新角色关联信息
        List<RoleResourceRef> userRoles = date.getUserRoles();
        if (CollectionUtils.isNotEmpty(userRoles)) {
            roleResourceRefService.batchUpdate(userRoles);
        }
        //更新存储信息
        List<StorageDto> storages = date.getStorages();
        if (CollectionUtils.isNotEmpty(storages)) {
            storages.forEach(storage -> {
                Storage storageInfo = configurationManagementService.getMutableConfigurationClone().getStorage(storage.getId());
                if (storageInfo == null) {
                    try {
                        configurationManagementService.createStorage(storage);
                    } catch (IOException e) {
                        log.error("创建存储失败", e);
                    }
                }else {
                    try {
                        configurationManagementService.updateStorage(storage);
                    } catch (IOException e) {
                        log.error("更新存储失败", e);
                    }
                }
            });
        }
        List<RepositoryDto> repositorys = date.getRepositorys();
        if (CollectionUtils.isNotEmpty(repositorys)) {
            repositorys.forEach(repository -> {
                String storageId = repository.getStorage().getId();
                String repositoryId = repository.getId();
                StorageDto storageDto = configurationManagementService.getMutableConfigurationClone().getStorage(storageId);
                if (storageDto == null) {
                    try {
                        configurationManagementService.createStorage(storageDto);
                    } catch (IOException e) {
                        log.error("创建仓库关联的存储失败", e);
                    }
                }
                Repository existRepository = storageDto.getRepository(repositoryId);
                boolean result = Objects.nonNull(existRepository) && (!repository.getLayout().equals(existRepository.getLayout()) || (Objects.nonNull(existRepository.getSubLayout()) && !existRepository.getSubLayout().equals(repository.getSubLayout())));
                if (!result) {
                    try {
                        //判断重复
                        configurationManagementService.addOrUpdateRepository(storageId, repository);
                    } catch (Exception e) {
                        log.error("新增、更新仓库失败", e);
                    }
                }
            });
        }
        //清理已删除的用户权限信息
        removeUserAuth(date);
    }

    /**
     *
     * 方法描述:  清理已删除的用户资源
     *
     * @param: 已删除的用户资源
     */
    private void removeUserAuth(UserAuthDTO date) {
        //删除用户信息
        List<String> removeUserIds = date.getRemoveUserIds();
        if (CollectionUtils.isNotEmpty(removeUserIds)) {
            String userId = removeUserIds.get(0);
            folibUserService.deleteByUserName(userId);
        }
        //删除用户组信息
        List<Long> removeGroupIds = date.getRemoveGroupIds();
        if (CollectionUtils.isNotEmpty(removeGroupIds)) {
            Long groupId = removeGroupIds.get(0);
            userGroupService.deleteById(groupId);
        }
        //删除角色信息
        List<String> removeRoleIds = date.getRemoveRoleIds();
        if (CollectionUtils.isNotEmpty(removeRoleIds)) {
            String roleId = removeRoleIds.get(0);
            folibRoleService.deleteById(roleId);
        }
        //删除资源信息
        List<String> removeResourceIds = date.getRemoveResourceIds();
        if (CollectionUtils.isNotEmpty(removeResourceIds)) {
            resourceService.deleteByIds(removeResourceIds);
            roleResourceRefService.deleteByResourceIds(removeResourceIds);
        }
    }

}
