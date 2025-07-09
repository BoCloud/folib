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
import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import com.folib.authorization.AuthorizationConfigFileManager;
import com.folib.authorization.dto.AuthorizationConfigDto;
import com.folib.authorization.dto.RoleDto;
import com.folib.components.DistributedCacheComponent;
import com.folib.constant.GlobalConstants;
import com.folib.mapper.FolibRoleMapper;
import com.folib.storage.repository.RepositoryPermissionDto;
import com.folib.users.domain.Privileges;
import com.folib.users.domain.SystemRole;
import com.folib.users.dto.AccessModelDto;
import com.folib.users.dto.RepositoryPrivilegesDto;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


import javax.inject.Inject;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 角色信息;(folib_role)表服务实现类
 * @author veadan
 * @date : 2024-7-17
 */
@Slf4j
@Service
@Transactional(rollbackFor=Exception.class)
public class FolibRoleServiceImpl implements FolibRoleService {
    @Lazy
    @Autowired
    private FolibRoleMapper folibRoleMapper;
    @Lazy
    @Inject
    private AuthorizationConfigFileManager authorizationConfigFileManager;
    @Lazy
    @Inject
    private ResourceService resourceService;
    @Lazy
    @Inject
    private RoleResourceRefService roleResourceRefService;
    @Lazy
    @Inject
    private DistributedCacheComponent distributedCacheComponent;
    @Lazy
    @Inject
    private UserGroupRefService userGroupRefService;
    @Lazy
    @Inject
    private UserGroupService userGroupService;
    @Override
    public FolibRole queryByRoleId(List<String> roleIds) {
        return null;
    }

    @Override
    public void syncYamlAuthorizationConfig() {
        try {
            AuthorizationConfigDto read = authorizationConfigFileManager.read();
            Set<RoleDto> roles = read.getRoles();
            roles = roles.stream().filter(roleDto -> !roleDto.getName().equalsIgnoreCase("CUSTOM_ROLE")
                    && !roleDto.getName().equalsIgnoreCase("GLOBAL_CONFIGURATION_MANAGER")
                    && !roleDto.getName().equalsIgnoreCase("USER_MANAGER")
                    && !roleDto.getName().equalsIgnoreCase("REPOSITORY_MANAGER") && !roleDto.getName().equalsIgnoreCase("TOKEN_MANAGER")
                    && !roleDto.getName().equalsIgnoreCase("LOGS_MANAGER") && !roleDto.getName().equalsIgnoreCase("UI_MANAGER")
            ).collect(Collectors.toSet());
            if(CollectionUtils.isNotEmpty(roles)) {
                List<FolibRole> folibRoles = new ArrayList<>(roles.size());
                List<RoleResourceRef> pathPrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> storagePrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> repositoryPrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> apiUserPrivilegeRoles = new ArrayList<>();
                List<RoleResourceRef> allRefs = new ArrayList<>();
                List<Resource> resources = new ArrayList<>();

                roles.forEach(roleDto -> {
                    String isDefault = GlobalConstants.NOT_DEFAULT;
                    if (SystemRole.ADMIN.name().equalsIgnoreCase(roleDto.getName()) || SystemRole.OPEN_SOURCE_MANAGE.name().equalsIgnoreCase(roleDto.getName()) || SystemRole.GENERAL.name().equalsIgnoreCase(roleDto.getName()) ) {
                        isDefault = GlobalConstants.DEFALUT;
                    }
                    folibRoles.add(FolibRole.builder().id(roleDto.getName()).description(roleDto.getDescription())
                            .enName(roleDto.getName()).deleted(GlobalConstants.NOT_DELETED).isDefault(isDefault).cnName(roleDto.getDescription()).build());
                    if(!"admin".equalsIgnoreCase(roleDto.getName())){
                        AccessModelDto accessModel = roleDto.getAccessModel();
                        if(accessModel != null) {
                            accessModel.getApiAuthorities().forEach(privileges -> apiUserPrivilegeRoles.add(RoleResourceRef.builder().roleId(roleDto.getName()).resourceType(GlobalConstants.RESOURCE_TYPE_API).apiAuthoritie(privileges.getAuthority()).build()));

                            accessModel.getStorageAuthorities().forEach(storagePrivilegesDto -> {
                                resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).build());
                                Set<Privileges> storagePrivileges = storagePrivilegesDto.getStoragePrivileges();
                                if(CollectionUtils.isNotEmpty(storagePrivileges)){
                                    List<RoleResourceRef> storagePrivilegeRef = storagePrivileges.stream().map(privilege ->
                                            RoleResourceRef.builder().roleId(roleDto.getName()).storageId(storagePrivilegesDto.getStorageId()).storagePrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()).collect(Collectors.toList());
                                    storagePrivilegeRoles.addAll(storagePrivilegeRef);
                                }

                                Set<RepositoryPrivilegesDto> repositorytories = storagePrivilegesDto.getRepositoryPrivileges();
                                if(CollectionUtils.isNotEmpty(repositorytories)){
                                    repositorytories.forEach(repositoryPrivilegesDto -> {
                                        resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).build());
                                        Set<Privileges> repositoryPrivileges = repositoryPrivilegesDto.getRepositoryPrivileges();
                                        List<RoleResourceRef> repositoryRef = repositoryPrivileges.stream().map(privilege ->
                                                RoleResourceRef.builder().roleId(roleDto.getName()).storageId(storagePrivilegesDto.getStorageId()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).repositoryPrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY).build()).collect(Collectors.toList());
                                        repositoryPrivilegeRoles.addAll(repositoryRef);

                                        repositoryPrivilegesDto.getPathPrivileges().forEach(pathPrivilegesDto -> {
                                            resources.add(Resource.builder().storageId(storagePrivilegesDto.getStorageId()).repositoryId(repositoryPrivilegesDto.getRepositoryId()).path(pathPrivilegesDto.getPath()).build());
                                            Set<Privileges> privileges = pathPrivilegesDto.getPrivileges();
                                            List<RoleResourceRef> pathRef = privileges.stream().map(privilege ->
                                                    RoleResourceRef.builder().roleId(roleDto.getName()).path(pathPrivilegesDto.getPath()).pathPrivilege(String.valueOf(privilege)).resourceType(GlobalConstants.RESOURCE_TYPE_PATH).build()).collect(Collectors.toList());
                                            pathPrivilegeRoles.addAll(pathRef);
                                        });
                                    });
                                }
                            });
                        }
                    }
                });
                //admin权限补全
                for (Privileges value : Privileges.values()) {
                    resources.add(Resource.builder().apiAuthoritie(value.getAuthority()).build());
                }

                //角色入库
                if(CollectionUtils.isNotEmpty(folibRoles)){
                    folibRoleMapper.insertOrUpdateBatch(folibRoles);
                }
                //资源入库
                List<Resource> resourceList = filterResource(resources);
                if(CollectionUtils.isNotEmpty(resourceList)) {
                    resourceService.saveBatch(resourceList.stream().distinct().collect(Collectors.toList()));

                    Map<String, Resource> pathMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getPath())).collect(Collectors.toMap(Resource::getPath, resource -> resource, (k1, k2)->k1));
                    Map<String, Resource> storageMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getStorageId())).collect(Collectors.toMap(Resource::getStorageId, resource -> resource, (k1,k2)->k1));
                    Map<String, Resource> repositoryMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getRepositoryId())).collect(Collectors.toMap(Resource::getRepositoryId, resource -> resource, (k1,k2)->k1));
                    Map<String, Resource> apiMap = resources.stream().filter(resource -> StringUtils.isNotEmpty(resource.getApiAuthoritie())).collect(Collectors.toMap(Resource::getApiAuthoritie, resource -> resource, (k1,k2)->k1));
                    //权限入库
                    List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryRefsByRoleIds(folibRoles.stream().map(FolibRole::getId).collect(Collectors.toList()));
                    Map<String, List<RoleResourceRef>> userRoles = roleResourceRefs.stream().collect(Collectors.groupingBy(RoleResourceRef::getRoleId));

                    storagePrivilegeRoles.forEach(roleResourceRef -> {
                        roleResourceRef.setRoleId(roleResourceRef.getRoleId());
                        roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                        roleResourceRef.setStoragePrivilege(roleResourceRef.getStoragePrivilege());
                        if (storageMap.containsKey(roleResourceRef.getStorageId())) {
                            roleResourceRef.setResourceId(storageMap.get(roleResourceRef.getStorageId()).getId());
                        }
                        allRefs.add(roleResourceRef);

                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setRoleId(ref.getRoleId());
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_STORAGE);
                                roleResourceRef.setStoragePrivilege(ref.getStoragePrivilege());
                                if (storageMap.containsKey(roleResourceRef.getStorageId())) {
                                    roleResourceRef.setResourceId(storageMap.get(roleResourceRef.getStorageId()).getId());
                                }
                                allRefs.add(roleResourceRef);
                            });
                        }
                    });

                    repositoryPrivilegeRoles.forEach(roleResourceRef -> {
                        roleResourceRef.setRoleId(roleResourceRef.getRoleId());
                        roleResourceRef.setRepositoryPrivilege(roleResourceRef.getRepositoryPrivilege());
                        roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                        if (repositoryMap.containsKey(roleResourceRef.getRepositoryId())) {
                            roleResourceRef.setResourceId(repositoryMap.get(roleResourceRef.getRepositoryId()).getId());
                        }
                        allRefs.add(roleResourceRef);

                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setRoleId(ref.getRoleId());
                                roleResourceRef.setRepositoryPrivilege(ref.getRepositoryPrivilege());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_REPOSITORY);
                                if (repositoryMap.containsKey(roleResourceRef.getRepositoryId())) {
                                    roleResourceRef.setResourceId(repositoryMap.get(roleResourceRef.getRepositoryId()).getId());
                                }
                                allRefs.add(roleResourceRef);
                            });
                        }
                    });

                    pathPrivilegeRoles.forEach(roleResourceRef -> {
                        roleResourceRef.setRoleId(roleResourceRef.getRoleId());
                        roleResourceRef.setPathPrivilege(roleResourceRef.getPathPrivilege());
                        roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                        if (pathMap.containsKey(roleResourceRef.getPath())) {
                            roleResourceRef.setResourceId(pathMap.get(roleResourceRef.getPath()).getId());
                        }
                        allRefs.add(roleResourceRef);

                        if (userRoles.containsKey(roleResourceRef.getRoleId())) {
                            userRoles.get(roleResourceRef.getRoleId()).forEach(ref -> {
                                roleResourceRef.setEntityId(ref.getEntityId());
                                roleResourceRef.setRefType(ref.getRefType());
                                roleResourceRef.setRoleId(ref.getRoleId());
                                roleResourceRef.setPathPrivilege(ref.getPathPrivilege());
                                roleResourceRef.setResourceType(GlobalConstants.RESOURCE_TYPE_PATH);
                                if (pathMap.containsKey(roleResourceRef.getPath())) {
                                    roleResourceRef.setResourceId(pathMap.get(roleResourceRef.getPath()).getId());
                                }
                                allRefs.add(roleResourceRef);
                            });
                        }
                    });

                    allRefs.addAll(apiUserPrivilegeRoles);

                    if (CollectionUtils.isNotEmpty(allRefs)) {
                        List<Resource> allResources = resourceService.findAll();
                        Map<String, Resource> resourceMap = allResources.stream().flatMap(resource -> Stream.of(
                                new AbstractMap.SimpleEntry<>(resource.getApiAuthoritie(), resource),
                                new AbstractMap.SimpleEntry<>(resource.getStorageId(), resource),
                                new AbstractMap.SimpleEntry<>(resource.getRepositoryId(), resource),
                                new AbstractMap.SimpleEntry<>(resource.getPath(), resource)
                        )).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (v1, v2) -> v1));
                        List<RoleResourceRef> collect = allRefs.stream().peek(roleResourceRef -> {
                            String apiAuthoritie = roleResourceRef.getApiAuthoritie();
                            String path = roleResourceRef.getPath();
                            String repositoryId = roleResourceRef.getRepositoryId();
                            String storageId = roleResourceRef.getStorageId();
                            if (StringUtils.isNotEmpty(apiAuthoritie)) {
                                roleResourceRef.setResourceId(resourceMap.get(apiAuthoritie).getId());
                            } else if (StringUtils.isNotEmpty(path)) {
                                roleResourceRef.setResourceId(resourceMap.get(path).getId());
                            } else if (StringUtils.isNotEmpty(repositoryId)) {
                                roleResourceRef.setResourceId(resourceMap.get(repositoryId).getId());
                            } else if (StringUtils.isNotEmpty(storageId)) {
                                roleResourceRef.setResourceId(resourceMap.get(storageId).getId());
                            }
                        }).collect(Collectors.toList());

                        roleResourceRefService.saveBath(collect);
                    }
                    //清理未关联资源的用户权限
                    List<Long> removeRefIds = roleResourceRefs.stream().filter(ref -> StringUtils.isNotEmpty(ref.getResourceType())).map(RoleResourceRef::getId).collect(Collectors.toList());
                    if(CollectionUtils.isNotEmpty(removeRefIds)) {
                        roleResourceRefService.removeByIds(removeRefIds);
                    }
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("获取yaml权限配置异常", e);
        }
    }

    private List<Resource> filterResource(List<Resource> resourceList) {
        List<Resource> resources = resourceService.findAll();
        List<String> storagetIds = resources.stream().map(Resource::getStorageId).filter(StringUtils::isNotEmpty).collect(Collectors.toList());
        List<String> repositoryIds = resources.stream().map(Resource::getRepositoryId).filter(StringUtils::isNotEmpty).collect(Collectors.toList());
        List<String> paths = resources.stream().filter(s -> StringUtils.isNotEmpty(s.getPath())).map(Resource::getPath).collect(Collectors.toList());
        List<String> apis = resources.stream().filter(s -> StringUtils.isNotEmpty(s.getApiAuthoritie())).map(Resource::getApiAuthoritie).collect(Collectors.toList());

        return resourceList.stream().filter(res -> {
            String repositoryId = res.getRepositoryId();
            String storageId1 = res.getStorageId();
            String apiAuthoritie = res.getApiAuthoritie();
            String path = res.getPath();

            return !((repositoryId != null && repositoryIds.contains(repositoryId) && storagetIds.contains(storageId1))
                    || (repositoryId == null && storagetIds.contains(storageId1))
                    || (StringUtils.isNotEmpty(apiAuthoritie) && apis.contains(apiAuthoritie))
                    || (StringUtils.isNotEmpty(path) && paths.contains(path)));
        }).collect(Collectors.toList());
    }

    /** 
     * 通过ID查询单条数据 
     *
     * @param id 主键
     * @return 实例对象
     */
    @Override
    public FolibRole queryById(String id){
        return folibRoleMapper.queryById(id);
    }
    
    /** 
     * 分页查询
     *
     * @param folibRole 筛选条件
     * @param pageRequest 分页对象
     * @return 查询结果
     */
    @Override
    public PageInfo<FolibRoleDTO> paginQuery(FolibRole folibRole, PageRequest pageRequest){
        PageHelper.startPage(pageRequest.getPageNumber(), pageRequest.getPageSize());
        List<FolibRoleDTO> folibRoleDTOS = folibRoleMapper.queryAllByLimit(folibRole);
        return new PageInfo<>(folibRoleDTOS);
    }
    
    /** 
     * 新增数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    @Override
    public FolibRole insert(FolibRole folibRole){
        folibRoleMapper.insert(folibRole);
        return folibRole;
    }
    
    /** 
     * 更新数据
     *
     * @param folibRole 实例对象
     * @return 实例对象
     */
    @Override
    public FolibRole update(FolibRole folibRole){
        folibRoleMapper.update(folibRole);
        return queryById(folibRole.getId());
    }
    
    /** 
     * 通过主键删除数据
     *
     * @param id 主键
     * @return 是否成功
     */
    @Override
    public boolean deleteById(String id){

        if (SystemRole.ADMIN.name().equalsIgnoreCase(id)) {
            throw new RuntimeException("Cannot delete the admin role");
        }

        int deleteNumber = folibRoleMapper.deleteById(id);

        roleResourceRefService.deleteByRoleId(id);
        return deleteNumber > 0;
    }

    @Override
    public void save(RoleDTO roleForm, String username) {

         List<AccessResourcesDTO> accessModel= roleForm.getResources();
        if(Objects.isNull(accessModel)){
            throw new RuntimeException("权限配置资源不能为空");
        }
        //保存角色信息
        FolibRole folibRole = queryById(roleForm.getName());
        if (folibRole != null) {
            return;
        }
        FolibRole roleInfo = FolibRole.builder().deleted(GlobalConstants.NOT_DELETED).isDefault(GlobalConstants.NOT_DEFAULT).id(roleForm.getName()).enName(roleForm.getName()).cnName(roleForm.getDescription()).description(roleForm.getDescription()).createBy(username).updateBy(username).build();
        insert(roleInfo);
        String roleId = roleInfo.getId();
        //保存权限关系
        roleResourceRefService.savePermissions(roleForm, roleId, username);

        List<AccessUsersDTO> users = null;
        if (Objects.nonNull(roleForm.getPrivileges())) {
            users = roleForm.getPrivileges().getUsers();
        }
        if (CollectionUtils.isNotEmpty(users)) {
            List<String> userIds = users.stream().map(AccessUsersDTO::getId).collect(Collectors.toList());
            deleteUserRoleCache(userIds);
        }
    }

    @Override
    public void updateRoleInfo(RoleDTO roleDTO, String roleId, String username) {
        List<AccessResourcesDTO> accessModel= roleDTO.getResources();
        if(Objects.isNull(accessModel)){
            throw new RuntimeException("权限配置资源不能为空");
        }

        //角色信息
        FolibRole folibRole = queryById(roleId);
        if (folibRole == null) {
            return;
        }
        FolibRole roleInfo = FolibRole.builder().id(roleId).enName(roleDTO.getName()).description(roleDTO.getDescription()).updateBy(username).build();
        update(roleInfo);

        Set<String> userIds;
        try {
            userIds = getUserIdsByRoleId(roleId);
        } catch (Exception e) {
            log.error("获取角色用户列表异常", e);
            throw new RuntimeException("Failed to update role permissions and clear user cache !");
        }
        roleResourceRefService.deleteByRoleId(roleId);
        List<AccessUsersDTO> users = null;
        if (Objects.nonNull(roleDTO.getPrivileges())) {
            users = roleDTO.getPrivileges().getUsers();
        }
        if (roleId.toUpperCase().startsWith("STORAGE_ADMIN_")) {
            //保存存储空间管理员权限
            if(Objects.isNull(users)){
                throw new RuntimeException("存储空间管理员用户不能为空");
            }
            String userId = users.get(0).getId();
            EnumSet<Privileges> storagePrivileges = Privileges.storageAll();
            Set<String> privileges = storagePrivileges.stream().map(Privileges::getAuthority).collect(Collectors.toSet());
            List<RoleResourceRef> roleResourceRefs = privileges.stream().map(privilege -> RoleResourceRef.builder().roleId(roleId).entityId(userId).refType(GlobalConstants.ROLE_TYPE_USER).resourceId(accessModel.get(0).getStorageId())
                    .storagePrivilege(privilege).resourceType(GlobalConstants.RESOURCE_TYPE_STORAGE).build()).collect(Collectors.toList());
            roleResourceRefService.saveBath(roleResourceRefs);
        }else{
            //保存权限关系
            roleResourceRefService.savePermissions(roleDTO, roleId, username);
        }

        if (CollectionUtils.isNotEmpty(users)) {
            userIds.addAll(users.stream().map(AccessUsersDTO::getId).collect(Collectors.toSet()));
        }

        if (SystemRole.ADMIN.name().equalsIgnoreCase(roleId) && !users.stream().map(AccessUsersDTO::getId).collect(Collectors.toList()).contains("admin")) {
            throw new IllegalArgumentException("admin user must have admin role");
        }
        if (CollectionUtils.isNotEmpty(userIds)) {
            deleteUserRoleCache(new ArrayList<>(userIds));
        }
    }

    private Set<String> getUserIdsByRoleId(String roleId) {
        //清理权限缓存
        List<RoleResourceRef> roleResourceRefs = roleResourceRefService.queryByRoleIds(Collections.singletonList(roleId));
        Set<String> userIds = new HashSet<>();
        if (CollectionUtils.isNotEmpty(roleResourceRefs)) {
            userIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER.equals(r.getRefType())).map(RoleResourceRef::getEntityId).collect(Collectors.toSet());
            List<Long> groupIds = roleResourceRefs.stream().filter(r -> GlobalConstants.ROLE_TYPE_USER_GROUP.equals(r.getRefType())).map(RoleResourceRef::getEntityId).map(Long::valueOf).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(groupIds)) {
                List<UserGroupRef> userGroupRefs = userGroupRefService.queryByGroupIds(groupIds);
                if (CollectionUtils.isNotEmpty(userGroupRefs)) {
                    userIds.addAll(userGroupRefs.stream().map(UserGroupRef::getUserId).collect(Collectors.toSet()));
                }
            }

        }
        return userIds;
    }

    @Override
    public void deleteUserRoleCache(List<String> userIds) {
        if (CollectionUtils.isNotEmpty(userIds)){
            userIds.forEach(userId -> {
                String roleKey = String.format("user_role_%s", userId);
                distributedCacheComponent.delete(roleKey);
                String userKey = String.format("user_%s", userId);
                distributedCacheComponent.delete(userKey);
            });
        }
    }

    @Override
    public List<FolibRole> queryRoles(FolibRole build) {
        return folibRoleMapper.selectList(Wrappers.<FolibRole>lambdaQuery().eq(FolibRole::getIsDefault,build.getIsDefault()));
    }

    @Override
    public RoleDTO getRoleDetail(String roleId, FolibRole folibRole) {
        RoleDTO roleDTO = RoleDTO.builder().description(folibRole.getDescription()).name(folibRole.getEnName()).build();
        //查角色关联的权限
        List<PermissionsDTO> permissions = roleResourceRefService.queryPermissions(roleId, null, null, null,false);
        //用户权限
        Map<String, List<PermissionsDTO>> userPermissions = permissions.stream().filter(permissionsDTO -> StringUtils.isNotEmpty(permissionsDTO.getRefType()) && GlobalConstants.ROLE_TYPE_USER.equals(permissionsDTO.getRefType())).collect(Collectors.groupingBy(PermissionsDTO::getEntityId));
        List<AccessUsersDTO> userAccess = userPermissions.entrySet().stream().map(entry -> {
            String userId = entry.getKey();
            List<PermissionsDTO> permissionDTOs = entry.getValue();
            List<String> collect = permissionDTOs.stream().flatMap(permissionDTO ->
                    Stream.of(permissionDTO.getRepositoryPrivilege(), permissionDTO.getStoragePrivilege(), permissionDTO.getPathPrivilege())).filter(StringUtils::isNotEmpty).distinct().collect(Collectors.toList());
            return AccessUsersDTO.builder().id(userId).access(collect).build();
        }).collect(Collectors.toList());

        //用户组权限
        Map<String, List<PermissionsDTO>> groupPermissions = permissions.stream().filter(permissionsDTO -> StringUtils.isNotEmpty(permissionsDTO.getRefType()) && GlobalConstants.ROLE_TYPE_USER_GROUP.equals(permissionsDTO.getRefType())).collect(Collectors.groupingBy(PermissionsDTO::getEntityId));
        List<Long> userGroupIds = Optional.ofNullable(groupPermissions).orElse(Collections.emptyMap()).keySet().stream().map(Long::valueOf).collect(Collectors.toList());
        List<UserGroup> userGroups = userGroupService.queryByIds(userGroupIds);
        Map<Long, String> groupMap = userGroups.stream().collect(Collectors.toMap(UserGroup::getId, UserGroup::getGroupName, (v1,v2) -> v1));
        List<AccessUserGroupsDTO> userGroupAccess = groupPermissions.entrySet().stream().map(entry -> {
            String groupId = entry.getKey();
            List<PermissionsDTO> permissionDTOs = entry.getValue();
            List<String> collect = permissionDTOs.stream().flatMap(permissionDTO ->
                    Stream.of(permissionDTO.getRepositoryPrivilege(), permissionDTO.getStoragePrivilege(), permissionDTO.getPathPrivilege())).filter(StringUtils::isNotEmpty).distinct().collect(Collectors.toList());
            return AccessUserGroupsDTO.builder().id(groupId).name(groupMap.get(Long.valueOf(groupId))).access(collect).build();
        }).collect(Collectors.toList());
        roleDTO.setPrivileges(AccessModelDTO.builder().users(userAccess).groups(userGroupAccess).build());

        //资源权限
        List<PermissionsDTO> resources = permissions.stream().filter(permissionsDTO -> StringUtils.isNotEmpty(permissionsDTO.getStorageId()) || StringUtils.isNotEmpty(permissionsDTO.getRepositoryId()) || StringUtils.isNotEmpty(permissionsDTO.getPath())).collect(Collectors.toList());
        List<AccessResourcesDTO> resourceList = resources.stream().map(resource ->
                AccessResourcesDTO.builder().resourceId(resource.getResourceId()).storageId(resource.getStorageId())
                        .repositoryId(resource.getRepositoryId()).path(resource.getPath()).build()
        ).distinct().collect(Collectors.toList());
        roleDTO.setResources(resourceList);

        List<PermissionsDTO> resourcePermissions = permissions.stream().filter(permissionsDTO -> StringUtils.isEmpty(permissionsDTO.getRefType())).collect(Collectors.toList());
        List<String> resourceAccess = Optional.of(resourcePermissions).orElse(new ArrayList<>()).stream().flatMap(resource -> Stream.of(resource.getRepositoryPrivilege(), resource.getStoragePrivilege(), resource.getPathPrivilege())).filter(StringUtils::isNotEmpty).distinct().collect(Collectors.toList());
        roleDTO.setAccess(resourceAccess);
        return roleDTO;
    }

    @Override
    public void deleteRole(String roleId) {
        if (SystemRole.ADMIN.name().equalsIgnoreCase(roleId)) {
            throw new RuntimeException("Cannot delete the admin role");
        }
        deleteById(roleId);
        //删除角色关联的用户缓存
        Set<String> userIds;
        try {
            userIds = getUserIdsByRoleId(roleId);
        } catch (Exception e) {
            log.error("获取角色用户列表异常", e);
            throw new RuntimeException(e);
        }
        deleteUserRoleCache(new ArrayList<>(userIds));
        //删除角色关联的资源权限
        roleResourceRefService.deleteByRoleId(roleId);
    }

    @Override
    public void saveOrUpdateBatch(List<FolibRole> roles) {
        folibRoleMapper.insertOrUpdateBatch(roles);
    }

    @Override
    public List<FolibRole> queryByIds(Set<String> roles) {
        return folibRoleMapper.selectList(Wrappers.<FolibRole>lambdaQuery().in(FolibRole::getId, roles));
    }

    @Override
    public void updateRepostoryPermission(String storageId, String repositoryId, RepositoryPermissionDto repositoryPermissionDto) {
        String roleId = String.format("%S_%S", storageId, repositoryId);
        FolibRole folibRole = queryById(roleId);
        List<AccessUsersDTO> users = new ArrayList<>();
        repositoryPermissionDto.getUserList().forEach(user -> {
            users.add(AccessUsersDTO.builder().id(user.getUsername()).access(user.getPermissions()).build());
        });

        AccessModelDTO privileges = AccessModelDTO.builder().users(users).build();
        RoleDTO roleDTO = RoleDTO.builder().name(roleId).description(roleId + "仓库默认角色").privileges(privileges).resources(Collections.singletonList(AccessResourcesDTO.builder().storageId(storageId).repositoryId(repositoryId).build())).build();
        if (folibRole == null) {
            save(roleDTO, null);
        }else {
            updateRoleInfo(roleDTO, roleId, null);
        }

    }


    /**
     * 获取仓库权限
     * @param repositoryAccess 仓库权限req
     * @param roleId 角色id
     * @param enetityId 用户、用户组id
     * @param roleType 角色类型
     * @param roleResourceRefs 角色权限关联列表
     */
    private void getRepositoriesAcces(RepositoryAccessModelDTO repositoryAccess, String roleId, String enetityId, String roleType, List<RoleResourceRef> roleResourceRefs) {
        String path = repositoryAccess.getPath();
        String repositoryId = repositoryAccess.getRepositoryId();
        String resourceType;
        if (StringUtils.isNotEmpty(path)) {
            resourceType = GlobalConstants.RESOURCE_TYPE_PATH;
        } else if (StringUtils.isNoneEmpty(repositoryId)) {
            resourceType = GlobalConstants.RESOURCE_TYPE_REPOSITORY;
        } else {
            resourceType = GlobalConstants.RESOURCE_TYPE_STORAGE;
        }
        repositoryAccess.getPrivileges().forEach(privilege -> {
            RoleResourceRef roleResourceRef;
            if (StringUtils.isNotEmpty(path)) {
                roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(enetityId).refType(roleType).resourceType(resourceType).resourceId(repositoryAccess.getResourceId()).pathPrivilege(privilege).build();
            } else if (StringUtils.isNoneEmpty(repositoryId)) {
                roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(enetityId).refType(roleType).resourceType(resourceType).resourceId(repositoryAccess.getResourceId()).repositoryPrivilege(privilege).build();
            } else {
                roleResourceRef = RoleResourceRef.builder().roleId(roleId).entityId(enetityId).refType(roleType).resourceType(resourceType).resourceId(repositoryAccess.getResourceId()).storagePrivilege(privilege).build();
            }
            roleResourceRefs.add(roleResourceRef);
        });
    }
}