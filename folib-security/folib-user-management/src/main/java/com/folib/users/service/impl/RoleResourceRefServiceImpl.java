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
import com.folib.dto.*;
import com.folib.entity.*;
import com.folib.users.service.*;
import com.google.common.collect.Lists;
import com.folib.constant.GlobalConstants;
import com.folib.converts.ResourceConvert;
import com.folib.mapper.RoleResourceRefMapper;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.dto.UserPermissionDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 权限表;(role_resource_ref)表服务实现类
 * @author veadan
 * @date : 2024-7-18
 */
@Service
@Transactional(rollbackFor=Exception.class)
public class RoleResourceRefServiceImpl implements RoleResourceRefService {
    @Autowired
    private RoleResourceRefMapper roleResourceRefMapper;
    @Autowired
    @Lazy
    private ResourceService resourceService;
    @Lazy
    @Autowired
    private UserGroupRefService userGroupRefService;
    @Lazy
    @Autowired
    private FolibUserService folibUserService;
    @Lazy
    @Autowired
    private FolibRoleService folibRoleService;
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
    public RoleResourceRef queryById(Long id){
        return roleResourceRefMapper.queryById(id);
    }
    
    ///**
    // * 分页查询
    // *
    // * @param roleResourceRef 筛选条件
    // * @param pageRequest 分页对象
    // * @return 查询结果
    // */
    //@Override
    //public Page<RoleResourceRef> paginQuery(RoleResourceRef roleResourceRef, PageRequest pageRequest){
    //    long total = roleResourceRefMapper.count(roleResourceRef);
    //    return new PageImpl<>(roleResourceRefMapper.queryAllByLimit(roleResourceRef, pageRequest), pageRequest, total);
    //}
    
    /** 
     * 新增数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    @Override
    public RoleResourceRef insert(RoleResourceRef roleResourceRef){
        roleResourceRefMapper.insert(roleResourceRef);
        String entityId = roleResourceRef.getEntityId();
        if (GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())){
            folibRoleService.deleteUserRoleCache(Collections.singletonList(entityId));
        }else if (GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())){
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(Collections.singletonList(Long.valueOf(entityId)));
            if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                folibRoleService.deleteUserRoleCache(userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toList()));
            }
        }
        return roleResourceRef;
    }
    
    /** 
     * 更新数据
     *
     * @param roleResourceRef 实例对象
     * @return 实例对象
     */
    @Override
    public RoleResourceRef update(RoleResourceRef roleResourceRef){
        roleResourceRefMapper.update(roleResourceRef);
        String entityId = roleResourceRef.getEntityId();
        if (GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())){
            folibRoleService.deleteUserRoleCache(Collections.singletonList(entityId));
        }else if (GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())){
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(Collections.singletonList(Long.valueOf(entityId)));
            if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                folibRoleService.deleteUserRoleCache(userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toList()));
            }
        }
        return queryById(roleResourceRef.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(Long id){
        RoleResourceRef roleResourceRef = queryById(id);
        if (roleResourceRef == null) {
            return true;
        }

        String entityId = roleResourceRef.getEntityId();
        if (GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())){
            String roleId = roleResourceRef.getRoleId();
            if (SystemRole.ADMIN.name().equalsIgnoreCase(roleId) &&  "admin".equalsIgnoreCase(entityId)) {
                throw new RuntimeException("Cannot delete the admin role");
            }

            folibRoleService.deleteUserRoleCache(Collections.singletonList(entityId));
        }else if (GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())){
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(Collections.singletonList(Long.valueOf(entityId)));
            if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                folibRoleService.deleteUserRoleCache(userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toList()));
            }
        }
        int total = roleResourceRefMapper.deleteById(id);
        return total > 0;
    }

    @Override
    public boolean deleteByRoleId(String roleId){
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(Collections.singletonList(roleId));
        List<RoleResourceRef> roleResourceRefs1 = roleResourceRefs.stream().filter(r -> StringUtils.isNotEmpty(r.getRefType())).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(roleResourceRefs1)) {
            List<String> userIds = roleResourceRefs1.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            List<Long> groupIds = roleResourceRefs1.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIds)){
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                    userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
                }
            }
            folibRoleService.deleteUserRoleCache(userIds);
        }

        List<Long> refIds;
        List<String> roleNames = Lists.newArrayList(SystemRole.READERS.name(), SystemRole.ARTIFACTS_MANAGER.name(), SystemRole.GENERAL.name(), SystemRole.OPEN_SOURCE_MANAGE.name());
        if (roleNames.stream().anyMatch(item -> item.equalsIgnoreCase(roleId))) {
            refIds = roleResourceRefs.stream().filter(r -> StringUtils.isNotEmpty(r.getRefType())).map(RoleResourceRef::getId).collect(Collectors.toList());
        } else if (SystemRole.ANONYMOUS.name().equalsIgnoreCase(roleId)) {
            ArrayList<String> privileges = Lists.newArrayList(Privileges.ARTIFACTS_RESOLVE.name(), Privileges.SEARCH_ARTIFACTS.name(), Privileges.ARTIFACTS_VIEW.name(), Privileges.CONFIGURATION_VIEW_METADATA_CONFIGURATION.name());
            refIds = roleResourceRefs.stream().filter(ref -> !privileges.contains(ref.getResourceId())).map(RoleResourceRef::getId).collect(Collectors.toList());
        } else {
            refIds = roleResourceRefs.stream().map(RoleResourceRef::getId).collect(Collectors.toList());
        }
        if  (CollectionUtils.isNotEmpty(refIds)) {
            int total = roleResourceRefMapper.deleteByRefIds(refIds);
            return total > 0;
        }
        return true;
    }

    @Override
    public List<RoleResourceRef> queryRoleByUserId(String uuid, List<String> roles) {
        return roleResourceRefMapper.queryRoleByUserId(uuid, roles);
    }

    @Override
    public int saveBath(List<RoleResourceRef> roleResourceRefs) {
        int number = roleResourceRefMapper.insertBatch(roleResourceRefs);
        List<String> userIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());

        List<Long> groupIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(groupIds)){
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
            }
        }
        folibRoleService.deleteUserRoleCache(userIds);
        return number;
    }

    @Override
    public List<RoleResourceRef> queryRefs(RoleResourceRef roleResourceRef) {
        return roleResourceRefMapper.selectList(Wrappers.<RoleResourceRef>lambdaQuery()
                .eq(roleResourceRef.getRoleId()!=null,RoleResourceRef::getRoleId, roleResourceRef.getRoleId())
                .eq(roleResourceRef.getRefType()!=null,RoleResourceRef::getRefType, roleResourceRef.getRefType())
                .eq(roleResourceRef.getEntityId()!=null,RoleResourceRef::getEntityId, roleResourceRef.getEntityId())
        );
    }

    @Override
    public List<RoleResourceRef> queryRefsByRoleIds(List<String> roleIds) {
        if (CollectionUtils.isEmpty(roleIds)) {
            return Lists.newArrayList();
        }
        return this.roleResourceRefMapper.queryAllByRoleId(roleIds);
    }

    @Override
    public void removeByIds(List<Long> removeRefIds) {
        deleteByIds(removeRefIds);
    }

    /**
     * 根据用户id查询关联的角色权限
     * @param userName 用户名
     */
    @Override
    public List<UserRoleDTO> getRolesByUserName(String userName) {
        List<String> roleIdsList = getRoleListByUserName(userName);
        if (CollectionUtils.isEmpty(roleIdsList)) {
            return null;
        }
        return roleResourceRefMapper.queryPrivileges(roleIdsList);
    }

    /**
     * 根据用户名查询角色id
     * @param userName
     * @return 角色关联列表
     */
    private List<String> getRoleListByUserName(String userName) {
        if (StringUtils.isEmpty(userName)) {
            return null;
        }
        List<UserGroupRef> userGroupRefs = userGroupRefService.queryByUserId(userName);
        Set<Long> groupIds = null;
        if (CollectionUtils.isNotEmpty(userGroupRefs)) {
            groupIds = userGroupRefs.stream().map(UserGroupRef::getUserGroupId).collect(Collectors.toSet());
        }

        List<RoleResourceRef> roleResourceRefs = roleResourceRefMapper.selectList(Wrappers.<RoleResourceRef>lambdaQuery()
                .eq(RoleResourceRef::getRefType, GlobalConstants.ROLE_TYPE_USER)
                .eq(RoleResourceRef::getEntityId, userName)
                .or(CollectionUtils.isNotEmpty(groupIds))
                .eq(CollectionUtils.isNotEmpty(groupIds),RoleResourceRef::getRefType, GlobalConstants.ROLE_TYPE_USER_GROUP)
                .eq(CollectionUtils.isNotEmpty(groupIds),RoleResourceRef::getEntityId, groupIds)
        );
        return roleResourceRefs.stream().map(RoleResourceRef::getRoleId).distinct().collect(Collectors.toList());
    }

    @Override
    public RoleDTO getUserByRoleId(String roleId) {
        return roleResourceRefMapper.getUserByRoleId(roleId);
    }

    @Override
    public List<PermissionsDTO> queryPermissions(String roleId, String username, String storageId, String repositoryId) {
        return roleResourceRefMapper.queryPermissions(roleId, username, storageId, repositoryId, null,null, true);
    }

    @Override
    public List<PermissionsDTO> queryPermissions(String roleId, String username, String storageId, String repositoryId, boolean resourceEmpty) {
        return roleResourceRefMapper.queryPermissions(roleId, username, storageId, repositoryId, null, null, resourceEmpty);
    }

    @Override
    public List<PermissionsDTO> queryPermissionsByResourceIds(List<String> resourceIds) {
        return roleResourceRefMapper.queryPermissions(null, null, null, null, null, resourceIds, false);
    }

    @Override
    public List<PermissionsDTO> queryPermissionsByStorageIds(List<String> storageIds) {
        return roleResourceRefMapper.queryPermissions(null, null, null, null, storageIds, null, false);
    }

    @Override
    public void savePermissions(RoleDTO roleForm, String roleId, String username) {
        List<AccessResourcesDTO> formResources = roleForm.getResources();
        List<Resource> resources = ResourceConvert.INSTANCE.formToDtoS(formResources);
        if (CollectionUtils.isNotEmpty(resources)) {
            List<Resource> allResource = resourceService.findResources(resources);
            List<Resource> addResources = resources.stream().filter(resource -> allResource.stream().noneMatch(resource1 -> resource.getStorageId().equals(resource1.getStorageId()) && Objects.equals(resource.getRepositoryId(), resource1.getRepositoryId()) && Objects.equals(resource.getPath(), resource1.getPath()))).collect(Collectors.toList());
            //资源不存在则创建
            if (CollectionUtils.isNotEmpty(addResources)) {
                resourceService.saveBatch(addResources);
                allResource.addAll(addResources);
            }

            Map<String, Resource> pathMap = allResource.stream().filter(resource -> !Objects.equals(resource.getPath(), null) && !resource.getPath().isEmpty()).collect(Collectors.toMap(resource -> resource.getStorageId() + "_" + resource.getRepositoryId() + "_" + resource.getPath(), resource -> resource, (k1, k2)->k1));
            Map<String, Resource> repositoryMap = allResource.stream().filter(resource -> Objects.equals(resource.getPath(), null) || resource.getPath().isEmpty()).filter(resource ->  !Objects.equals(resource.getRepositoryId(), null) && !resource.getRepositoryId().isEmpty()).collect(Collectors.toMap(resource -> resource.getStorageId() + "_" + resource.getRepositoryId(), resource -> resource));
            Map<String, Resource> storageMap = allResource.stream().filter(resource -> Objects.equals(resource.getPath(), null) || resource.getPath().isEmpty()).filter(resource ->  Objects.equals(resource.getRepositoryId(), null) || resource.getRepositoryId().isEmpty()).filter(resource -> !Objects.equals(resource.getStorageId(), null) && !resource.getStorageId().isEmpty()).collect(Collectors.toMap(Resource::getStorageId, resource -> resource));

            resources.forEach(resourceDTO -> {
                String resourceId = null;
                // 根据 path 查找
                if (resourceDTO.getPath() != null && !resourceDTO.getPath().isEmpty()) {
                    resourceId = pathMap.get(resourceDTO.getStorageId() + "_" + resourceDTO.getRepositoryId() + "_" + resourceDTO.getPath()).getId();
                }
                // 根据 repositoryId 查找
                if (resourceId == null && resourceDTO.getRepositoryId() != null) {
                    resourceId = repositoryMap.get(resourceDTO.getStorageId() + "_" + resourceDTO.getRepositoryId()).getId();
                }
                // 根据 storageId 查找
                if (resourceId == null && resourceDTO.getStorageId() != null) {
                    resourceId = storageMap.get(resourceDTO.getStorageId()).getId();
                }
                // 将 resourceId 赋值回 resourceDTO
                resourceDTO.setId(resourceId);
            });
        }


        //保存权限
        List<RoleResourceRef> roleResourceRefs = new ArrayList<>();
        AccessModelDTO privileges = roleForm.getPrivileges();
        List<String> resourceAccess = roleForm.getAccess();
        List<AccessUsersDTO> users = null;
        List<AccessUserGroupsDTO> groups = null;
        if (Objects.nonNull(privileges)) {
            users = privileges.getUsers();
            groups = privileges.getGroups();
        }
        if (CollectionUtils.isNotEmpty(resourceAccess)) {
            List<RoleResourceRef> roleResourceRef = resources.stream().flatMap(accessResourcesDTO -> resourceAccess.stream().map(access -> {
                if (StringUtils.isNotBlank(accessResourcesDTO.getPath())) {
                    return RoleResourceRef.builder().roleId(roleId).resourceId(accessResourcesDTO.getId()).pathPrivilege(access).createBy(username).build();
                } else if (StringUtils.isNotBlank(accessResourcesDTO.getRepositoryId())) {
                    return RoleResourceRef.builder().roleId(roleId).resourceId(accessResourcesDTO.getId()).repositoryPrivilege(access).createBy(username).build();
                }else {
                    return RoleResourceRef.builder().roleId(roleId).resourceId(accessResourcesDTO.getId()).storagePrivilege(access).createBy(username).build();
                }
            })).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(resources)){
                saveBath(roleResourceRef);
            }
        }
        if (CollectionUtils.isEmpty(users) && CollectionUtils.isEmpty(groups)){
            if (CollectionUtils.isNotEmpty(resources)) {
                List<RoleResourceRef> roleResourceRef = resources.stream().map(accessResourcesDTO -> RoleResourceRef.builder().roleId(roleId).resourceId(accessResourcesDTO.getId()).createBy(username).build()).collect(Collectors.toList());
                saveBath(roleResourceRef);
            }
            return;
        }

        //用户权限组装
        if (CollectionUtils.isNotEmpty(users)){
             users.forEach(user -> {
                 if (StringUtils.isNotEmpty(user.getId())) {
                     if (CollectionUtils.isNotEmpty(resources)) {
                         resources.forEach(accessResourcesDTO -> {
                             List<String> access = user.getAccess();
                             if (CollectionUtils.isNotEmpty(access)) {
                                 access.forEach(pri -> {
                                     if(StringUtils.isEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER)
                                                 .storagePrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                                     }

                                     if(StringUtils.isNotEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER)
                                                 .repositoryPrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build());
                                     }

                                     if (StringUtils.isNotEmpty(accessResourcesDTO.getPath())) {
                                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER).pathPrivilege(pri)
                                                 .resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build());
                                     }
                                 });
                             }else {
                                 roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER)
                                         .resourceId(accessResourcesDTO.getId()).createBy(username).build());
                             }
                         });
                     }else {
                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(user.getId()).refType(GlobalConstants.ROLE_TYPE_USER).createBy(username).build());
                     }
                 }
             });

        }
        //用户组权限组装
        if (CollectionUtils.isNotEmpty(groups)){
            List<String> groupNames = groups.stream().map(AccessUserGroupsDTO::getName).filter(StringUtils::isNotEmpty).distinct().collect(Collectors.toList());
            List<UserGroup> userGroups = userGroupService.queryByGroupNames(groupNames);
            Map<String, Long> groupMap = Optional.ofNullable(userGroups).orElse(Collections.emptyList()).stream().collect(Collectors.toMap(UserGroup::getGroupName, UserGroup::getId));
            groups.forEach(groupsDTO -> {
                 if ( StringUtils.isNotEmpty(groupsDTO.getId()) || StringUtils.isNotEmpty(groupsDTO.getName())) {
                     if (StringUtils.isEmpty(groupsDTO.getId())) {
                         groupsDTO.setId(Optional.ofNullable(groupMap.get(groupsDTO.getName())).map(String::valueOf).orElse(null));
                     }
                     if (CollectionUtils.isNotEmpty(resources)) {
                         resources.forEach(accessResourcesDTO ->{
                             List<String> access = groupsDTO.getAccess();
                             if (CollectionUtils.isNotEmpty(access)) {
                                 access.forEach(pri -> {
                                     if(StringUtils.isEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP)
                                                 .storagePrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build());
                                     }

                                     if(StringUtils.isNotEmpty(accessResourcesDTO.getRepositoryId()) && StringUtils.isEmpty(accessResourcesDTO.getPath())) {
                                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP)
                                                 .repositoryPrivilege(pri).resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build());
                                     }
                                     if(StringUtils.isNotEmpty(accessResourcesDTO.getPath())) {
                                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP).pathPrivilege(pri)
                                                 .resourceId(accessResourcesDTO.getId()).createBy(username).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build());
                                     }
                                 });
                             }else {
                                 roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP)
                                         .resourceId(accessResourcesDTO.getId()).createBy(username).build());
                             }
                         });
                     }else {
                         roleResourceRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(groupsDTO.getId()).refType(GlobalConstants.ROLE_TYPE_USER_GROUP).createBy(username).build());
                     }
                 }
             });
        }

        if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
            saveBath(roleResourceRefs);
        }
    }

    @Override
    public List<RoleResourceRef> queryApiAuthorities(List<String> roleIds) {
        return roleResourceRefMapper.queryApiAuthorities(roleIds);
    }

    @Override
    public void batchUpdate(List<RoleResourceRef> userRoles) {
        List<String> roleIds = userRoles.stream().map(RoleResourceRef::getRoleId).collect(Collectors.toList());
        List<RoleResourceRef> queryUserRoleRefs = roleResourceRefMapper.queryByRoleIds(roleIds);
        if (!queryUserRoleRefs.isEmpty()) {
            userRoles = userRoles.stream().filter(userRoleRef -> queryUserRoleRefs.stream().noneMatch(queryUserGroupRef ->
                    queryUserGroupRef.getRoleId().equals(userRoleRef.getRoleId())
                            && Objects.equals(queryUserGroupRef.getEntityId(), userRoleRef.getEntityId())
                            && Objects.equals(queryUserGroupRef.getRefType(), userRoleRef.getRefType())
                            && Objects.equals(queryUserGroupRef.getResourceId(), userRoleRef.getResourceId())
                            && Objects.equals(queryUserGroupRef.getResourceType(), userRoleRef.getResourceType())
                            && Objects.equals(queryUserGroupRef.getPathPrivilege(), userRoleRef.getPathPrivilege())
                            && Objects.equals(queryUserGroupRef.getRepositoryPrivilege(), userRoleRef.getRepositoryPrivilege())
                            && Objects.equals(queryUserGroupRef.getStoragePrivilege(), userRoleRef.getStoragePrivilege()))).collect(Collectors.toList());
        }
        if (CollectionUtils.isNotEmpty(userRoles)) {
            roleResourceRefMapper.insertBatch(userRoles);
            List<String> userIds = userRoles.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            List<Long> groupIds = userRoles.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIds)){
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                    userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
                }
            }
            folibRoleService.deleteUserRoleCache(userIds);
        }
    }

    @Override
    public List<RoleResourceRef> queryByRoleIds(List<String> roleIds) {
        return roleResourceRefMapper.queryByRoleIds(roleIds);
    }

    @Override
    public List<FolibUser> queryUserByRoleIds(String roleId) {
        List<RoleResourceRef> roleResourceRefs = roleResourceRefMapper.queryByRoleIds(Collections.singletonList(roleId));
        List<String> userIds = roleResourceRefs.stream().filter(roleResourceRef -> GlobalConstants.ROLE_TYPE_USER.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());

        List<Long> groupIds = roleResourceRefs.stream().filter(roleResourceRef -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(roleResourceRef.getRefType())).map(RoleResourceRef::getEntityId).map(Long::valueOf).collect(Collectors.toList());
        if(CollectionUtils.isNotEmpty(groupIds)) {
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
            if (CollectionUtils.isNotEmpty(userGroupRefs)){
                userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toList()));
            }
        }

        return folibUserService.queryByIds(userIds);
    }

    @Override
    public void deleteAllByRoleId(String roleId) {
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(Collections.singletonList(roleId));
        if (CollectionUtils.isNotEmpty(roleResourceRefs)){
            List<String> userIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            List<Long> groupIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIds)){
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                    userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
                }
            }
            folibRoleService.deleteUserRoleCache(userIds);
        }
        roleResourceRefMapper.delete(Wrappers.<RoleResourceRef>lambdaQuery().eq(RoleResourceRef::getRoleId, roleId).isNull(RoleResourceRef::getEntityId));
    }
    @Override
    public void deleteAnonymousRole(String roleId) {
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(Collections.singletonList(roleId));
        if (CollectionUtils.isNotEmpty(roleResourceRefs)){
            List<String> userIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            List<Long> groupIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIds)){
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                    userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
                }
            }
            folibRoleService.deleteUserRoleCache(userIds);
        }

        ArrayList<String> privileges = Lists.newArrayList(Privileges.ARTIFACTS_RESOLVE.name(), Privileges.SEARCH_ARTIFACTS.name(), Privileges.ARTIFACTS_VIEW.name(), Privileges.CONFIGURATION_VIEW_METADATA_CONFIGURATION.name());
        List<Long> refIds = roleResourceRefs.stream().filter(ref -> privileges.contains(ref.getResourceId())).map(RoleResourceRef::getId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(refIds)) {
            roleResourceRefMapper.deleteByRefIds(refIds);
        }
    }

    public void deleteAllByRoleIdAndEntityNotNull(String roleId){
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(Collections.singletonList(roleId));
        if (CollectionUtils.isNotEmpty(roleResourceRefs)){
            List<String> userIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            List<Long> groupIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIds)){
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                    userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
                }
            }
            folibRoleService.deleteUserRoleCache(userIds);
        }
        roleResourceRefMapper.delete(Wrappers.<RoleResourceRef>lambdaQuery().eq(RoleResourceRef::getRoleId, roleId));

    }

    @Override
    public void updateStorageUser(UserPermissionDTO userPermission) {
        if (CollectionUtils.isEmpty(userPermission.getRoleIds())) {
            return;
        }
        List<String> privileges = new ArrayList<>(userPermission.getPrivileges());
        List<String> roleIds = new ArrayList<>(userPermission.getRoleIds());
        String userId = userPermission.getUserId();
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(roleIds);
        if (CollectionUtils.isEmpty(roleResourceRefs)) {
            return;
        }
        //清理用户已关联的角色
        List<Long> removeRefIds = roleResourceRefs.stream().filter(ref -> GlobalConstants.ROLE_TYPE_USER.equals(ref.getRefType()) &&
                Objects.equals(userId, ref.getEntityId()) && !privileges.contains(ref.getStoragePrivilege())).map(RoleResourceRef::getId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(removeRefIds)) {
            deleteByIds(removeRefIds);
        }
        List<String> refRoleIds = roleResourceRefs.stream().filter(ref -> GlobalConstants.ROLE_TYPE_USER.equals(ref.getRefType()) &&
                Objects.equals(userId, ref.getEntityId()) && privileges.contains(ref.getStoragePrivilege())).map(RoleResourceRef::getRoleId).collect(Collectors.toList());
        roleIds.removeAll(refRoleIds);

        //保存关联权限
        if(CollectionUtils.isNotEmpty(roleIds)) {
            List<RoleResourceRef> updateRefs = new ArrayList<>();
            roleIds.forEach(roleId -> {
                if (CollectionUtils.isNotEmpty(privileges)) {
                    privileges.forEach(privilege -> {
                        updateRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).storagePrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).resourceId(roleId.replaceFirst("STORAGE_USER_", "")).build());
                    });
                }else {
                    updateRefs.add(RoleResourceRef.builder().roleId(roleId).entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE)
                            .resourceId(roleId.replaceFirst("STORAGE_USER_", "")).build());
                }
            });
            if (CollectionUtils.isNotEmpty(updateRefs)) {
                List<RoleResourceRef> roleResourceRefList = updateRefs.stream().distinct().collect(Collectors.toList());
                saveBath(roleResourceRefList);

                List<String> userIds = roleResourceRefList.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
                List<Long> groupIds = roleResourceRefList.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(r -> Long.valueOf(r.getEntityId())).collect(Collectors.toList());
                if (CollectionUtils.isNotEmpty(groupIds)){
                    List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                    if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                        userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList()));
                    }
                }
                folibRoleService.deleteUserRoleCache(userIds);

            }
        }

    }

    @Override
    public void deleteByResourceIds(List<String> resourceIds) {
        roleResourceRefMapper.delete(Wrappers.<RoleResourceRef>lambdaQuery().in(RoleResourceRef::getResourceId, resourceIds));
    }

    @Override
    public List<RoleResourceRef> queryByResourceIds(List<String> resourceIds) {
        return roleResourceRefMapper.selectList(Wrappers.<RoleResourceRef>lambdaQuery().in(RoleResourceRef::getResourceId, resourceIds));
    }

    @Override
    public void deleteByentityId(String entityId, String refType) {
        roleResourceRefMapper.delete(Wrappers.<RoleResourceRef>lambdaQuery().eq(RoleResourceRef::getEntityId, entityId).eq(RoleResourceRef::getRefType, refType));

        if (GlobalConstants.ROLE_TYPE_USER.equals(refType)) {
            folibRoleService.deleteUserRoleCache(Collections.singletonList(entityId));
        }else if (GlobalConstants.ROLE_TYPE_USER_GROUP.equals(refType)) {
            List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(Collections.singletonList(Long.valueOf(entityId)));
            List<String> userIds = userGroupRefs.stream().map(UserGroupRef::getUserId).distinct().collect(Collectors.toList());
            folibRoleService.deleteUserRoleCache(userIds);
        }
    }

    @Override
    public void deleteByIds(List<Long> removeIds) {
        roleResourceRefMapper.delete(Wrappers.<RoleResourceRef>lambdaUpdate().in(RoleResourceRef::getId, removeIds));

        List<RoleResourceRef> roleResourceRefs = queryByIds(removeIds);
        if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
            roleResourceRefs = roleResourceRefs.stream().filter(r -> !"admin".equalsIgnoreCase(r.getRoleId()) && GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType()) && !"admin".equalsIgnoreCase(r.getEntityId())).collect(Collectors.toList());
            List<String> userIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toList());
            folibRoleService.deleteUserRoleCache(userIds);
        }
    }

    @Override
    public List<RoleResourceRef> queryPermissionsByRoleIds(List<String> roleIds) {
        return roleResourceRefMapper.selectList(Wrappers.<RoleResourceRef>lambdaQuery().in(RoleResourceRef::getRoleId, roleIds));
    }

    @Override
    public List<RoleResourceRef> queryByUserIds(List<String> userIds) {
        return roleResourceRefMapper.selectList(Wrappers.<RoleResourceRef>lambdaQuery()
                .in(RoleResourceRef::getEntityId, userIds)
                .eq(RoleResourceRef::getRefType, GlobalConstants.ROLE_TYPE_USER));

    }

    @Override
    public void updateUserPermission(Set<UserPermissionDTO> userPermissions) {
        if (CollectionUtils.isEmpty(userPermissions)) {
            return;
        }
        Set<String> roleIds = userPermissions.stream().map(UserPermissionDTO::getRoleIds).flatMap(Collection::stream).collect(Collectors.toSet());
        Set<String> userIds = userPermissions.stream().map(UserPermissionDTO::getUserId).collect(Collectors.toSet());
        List<RoleResourceRef> roleResourceRefs = queryByRoleIds(new ArrayList<>(roleIds));
        if (CollectionUtils.isEmpty(roleResourceRefs)) {
            return;
        }
        //清理用户已关联的角色
        List<Long> removeRefIds = roleResourceRefs.stream().filter(ref -> GlobalConstants.ROLE_TYPE_USER.equals(ref.getRefType()) && userIds.contains(ref.getEntityId())).map(RoleResourceRef::getId).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(removeRefIds)) {
            deleteByIds(removeRefIds);
        }

        //保存关联权限
        Map<String, List<RoleResourceRef>> roleMap = roleResourceRefs.stream().filter(ref -> StringUtils.isNotEmpty(ref.getResourceType())).map(ref -> RoleResourceRef.builder().roleId(ref.getRoleId()).resourceId(ref.getResourceId()).resourceType(ref.getResourceType())
                .storagePrivilege(ref.getStoragePrivilege()).repositoryPrivilege(ref.getRepositoryPrivilege()).pathPrivilege(ref.getPathPrivilege()).build()).distinct().collect(Collectors.groupingBy(RoleResourceRef::getRoleId));
        List<RoleResourceRef> updateRefs = new ArrayList<>();
        userPermissions.forEach(userPermission -> {
            String userId = userPermission.getUserId();
            Collection<String> userRoleIds = userPermission.getRoleIds();
            Collection<String> privileges = userPermission.getPrivileges();
            userRoleIds.forEach(roleId -> {
                List<RoleResourceRef> resourceRefs = roleMap.get(roleId);
                if (CollectionUtils.isNotEmpty(resourceRefs)) {
                    resourceRefs.forEach(resourceRef -> privileges.forEach(privilege -> {
                        RoleResourceRef.RoleResourceRefBuilder builder = RoleResourceRef.builder();
                        switch(resourceRef.getResourceType()){
                            case GlobalConstants.RESOURCE_TYPE_STORAGE:
                                builder.storagePrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                                break;
                            case GlobalConstants.RESOURCE_TYPE_REPOSITORY:
                                builder.repositoryPrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                                break;
                            case GlobalConstants.RESOURCE_TYPE_PATH:
                                builder.pathPrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                                break;
                        }
                        builder.entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).roleId(roleId).resourceId(resourceRef.getResourceId());
                        updateRefs.add(builder.build());
                    }));
                }
            });
        });
        if (CollectionUtils.isNotEmpty(updateRefs)) {
            saveBath(updateRefs.stream().distinct().collect(Collectors.toList()));
        }
    }

    @Override
    public List<RoleResourceRef> queryByIds(List<Long> ids) {
       return roleResourceRefMapper.selectList(Wrappers.<RoleResourceRef>lambdaQuery().in(RoleResourceRef::getId, ids));
    }

    @Override
    public List<RoleResourceRef> queryResourcesByRoleIds(List<String> roleIds) {
        return roleResourceRefMapper.queryResourcesByRoleIds(roleIds);
    }
}